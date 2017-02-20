#include "hans/sequencer.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
#include <functional>
#include <iostream>
#include "hans/interpreter.hpp"

using namespace hans;
using namespace hans::sequencer;
using namespace hans::sequencer::detail;
using namespace std::chrono;

static void sleep_thread(float ms) {
  std::this_thread::sleep_for(microseconds(static_cast<long>(ms * 1000)));
}

GlobalState::GlobalState(Handler handler_) : handler(handler_) {
  stop.store(false);
  ready.store(false);
}

DoubleBuffer::DoubleBuffer() {
  swap.store(false);
}

DoubleBuffer::DoubleBuffer(const DoubleBuffer& other) {
  swap.store(other.swap.load());
  m_front = other.m_front;
  m_back = other.m_back;
}

EventList& DoubleBuffer::get(DoubleBuffer::Caller caller) {
  bool value = swap.load();
  if (caller == DoubleBuffer::Caller::READER) {
    return (value) ? m_back : m_front;
  }

  return (value) ? m_front : m_back;
}

void DoubleBuffer::clear() {
  m_front.clear();
  m_back.clear();
}

CycleClock::CycleClock() {
  m_start = high_resolution_clock::now();
  elapsed = 0;
}

void CycleClock::start() {
  m_start = high_resolution_clock::now();
  elapsed = 0;
}

void CycleClock::tick() {
  auto now = high_resolution_clock::now();
  elapsed = duration_cast<microseconds>(now - m_start).count() / 1000.f;
}

sequencer::detail::TrackState::TrackState(Track track)
    : cycle(0), primitive(track) {
  dispatched = 0;
}

// Put aside events outside of this cycle into a future list
static void get_current_events(EventList& output, EventList& future,
                               EventList& input, Cycle& cycle) {
  auto duration = cycle.duration;

  for (auto& event : input) {
    event.cycle = cycle.number;

    if (event.start < duration) {
      output.push_back(event);
      continue;
    }

    auto diff = event.start - duration;
    auto ahead = std::floor(diff / duration);

    event.cycle += ahead + 1;
    event.start = diff - (ahead * duration);
    future.push_back(event);
  }
}

// Add future events to the current list
static void add_future_events(EventList& out, EventList& future, Cycle& cycle) {
  auto moved = 0;

  std::sort(future.begin(), future.end(),
            [](const Event& a, const Event& b) { return a.cycle < b.cycle; });

  for (const auto& event : future) {
    if (event.cycle != cycle.number) {
      break;
    }
    out.push_back(event);
    moved++;
  }

  if (moved) {
    future.erase(future.begin(), future.begin() + moved);
  }
}

// Produce events to be dispatched in the next cycle.
static void _sequencer_eval_task(sequencer::detail::TrackState& track) {
  Interpreter itp(track.cycle, track.primitive.instructions);
  interpret(itp);
  auto value = itp.dstack.pop();
  auto events = interpreter::to_events(itp.cycle, value.tree);

  // Sort and filter events
  auto& buffer = track.buffer.get(DoubleBuffer::Caller::WRITER);
  get_current_events(buffer, track.future, events, track.cycle);
  add_future_events(buffer, track.future, track.cycle);
  std::sort(buffer.begin(), buffer.end(),
            [](const Event& a, const Event& b) { return a.start < b.start; });
  events.clear();

  // Swap the tracks buffers
  auto side = track.buffer.swap.load();
  track.buffer.swap.store(!side);
}

// Dispatch all start-events, adding them to the end-event list if needed
static void process_on_events(EventList& on_events,
                              sequencer::detail::TrackState& track,
                              Handler& handler) {
  auto length = on_events.size();

  while (track.dispatched < length) {
    auto& event = on_events.at(track.dispatched);
    if (track.clock.elapsed < event.start) {
      break;
    }

    handler(track.primitive, event.value, true);

    if (event.duration != 0) {
      track.off_events.push_back(event);
    }

    track.dispatched++;
  }
}

// Tick and dispatch end-events
static void process_off_events(sequencer::detail::TrackState& track,
                               float delta, Handler& handler) {
  auto removed = 0;

  std::sort(
      track.off_events.begin(), track.off_events.end(),
      [](const Event& a, const Event& b) { return a.duration < b.duration; });

  for (auto& event : track.off_events) {
    if (event.duration <= 0) {
      handler(track.primitive, event.value, false);
      removed++;
    } else {
      event.duration -= delta;
    }
  }

  if (removed) {
    track.off_events.erase(track.off_events.begin(),
                           track.off_events.begin() + removed);
  }
}

Sequencer::Sequencer(TaskQueue& task_queue, Handler handler,
                     Sequences& sequences)
    : _task_queue(task_queue), _global(handler), _sequences(sequences) {
  _program.store(0);
  for (const auto& track : _sequences.tracks) {
    _tracks.push_back(detail::TrackState(track));
  }
}

Sequencer::~Sequencer() {
  stop();
}

void Sequencer::set_program(uint32_t program) {
  _program.store(program);
}

void Sequencer::run_forever() {
  auto resolution = 0.12; /* milliseconds */

  for (auto& track : _tracks) {
    // Evaluate the first cycle
    _sequencer_eval_task(track);

    // Then start evaluating the next cycle
    track.cycle.number++;
    _task_queue.async(TaskQueue::SEQUENCER_EVAL,
                      [&]() { _sequencer_eval_task(track); });
  }

  for (auto& track : _tracks) {
    track.clock.start();
  }

  // Spin tracks independently
  while (!_global.stop.load()) {
    auto range = _sequences.ranges.at(_program.load());

    for (auto i = range.start; i < range.end; ++i) {
      auto& track = _tracks.at(i);
      track.clock.tick();

      auto& on_events = track.buffer.get(DoubleBuffer::Caller::READER);
      process_on_events(on_events, track, _global.handler);
      process_off_events(track, resolution, _global.handler);

      if (track.clock.elapsed >= track.cycle.duration) {
        on_events.clear();
        track.dispatched = 0;
        track.clock.start();
        track.cycle.number++;

        _task_queue.async(TaskQueue::SEQUENCER_EVAL,
                          [&]() { _sequencer_eval_task(track); });
      }
    }

    sleep_thread(resolution);
  }

  // Flush all off events
  for (auto& track : _tracks) {
    for (auto& event : track.off_events) {
      _global.handler(track.primitive, event.value, false);
    }
  }
}

bool Sequencer::stop() {
  _global.stop.store(true);

  for (auto& track : _tracks) {
    track.buffer.clear();
  }

  return true;
}
