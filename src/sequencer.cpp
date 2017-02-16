#include "hans/sequencer.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
#include <functional>
#include <iostream>

using namespace hans;
using namespace hans::sequencer;
using namespace hans::sequencer::detail;
using namespace std::chrono;

static const size_t sequencer_eval_tag = 1;

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

sequencer::detail::Track::Track(uint64_t _id, Callback callback)
    : id(_id), cycle(0), producer(callback) {
  dispatched = 0;
}

// Put aside events outside of this cycle into a future list
static void get_current_events(EventList& output, EventList& future,
                               EventList& input, Cycle& cycle) {
  auto duration = cycle.duration.load();

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
static void _sequencer_eval_task(sequencer::detail::Track& track) {
  auto events = track.producer(track.cycle);

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
                              sequencer::detail::Track& track,
                              Handler& handler) {
  auto length = on_events.size();

  while (track.dispatched < length) {
    auto& event = on_events.at(track.dispatched);
    if (track.clock.elapsed < event.start) {
      break;
    }

    handler(track.id, event.value, true);

    if (event.duration != 0) {
      track.off_events.push_back(event);
    }

    track.dispatched++;
  }
}

// Tick and dispatch end-events
static void process_off_events(sequencer::detail::Track& track, float delta,
                               Handler& handler) {
  auto removed = 0;

  std::sort(
      track.off_events.begin(), track.off_events.end(),
      [](const Event& a, const Event& b) { return a.duration < b.duration; });

  for (auto& event : track.off_events) {
    if (event.duration <= 0) {
      handler(track.id, event.value, false);
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

Sequencer::Sequencer(TaskQueue& task_queue, Handler handler)
    : _task_queue(task_queue), _global(handler) {
}

Sequencer::~Sequencer() {
  stop();
}

size_t Sequencer::add_track(Callback callback) {
  auto id = _tracks.size();
  auto track = detail::Track(id, callback);
  _tracks.push_back(std::move(track));
  return id;
}

void Sequencer::run_forever() {
  auto resolution = 0.12; /* milliseconds */

  for (auto& track : _tracks) {
    // Evaluate the first cycle
    _sequencer_eval_task(track);

    // Then start evaluating the next cycle
    track.cycle.number++;
    _task_queue.async(sequencer_eval_tag,
                      [&]() { _sequencer_eval_task(track); });
  }

  for (auto& track : _tracks) {
    track.clock.start();
  }

  // Spin tracks independently
  while (!_global.stop.load()) {
    for (auto& track : _tracks) {
      track.clock.tick();

      auto& on_events = track.buffer.get(DoubleBuffer::Caller::READER);
      process_on_events(on_events, track, _global.handler);
      process_off_events(track, resolution, _global.handler);

      if (track.clock.elapsed >= track.cycle.duration.load()) {
        on_events.clear();
        track.dispatched = 0;
        track.clock.start();
        track.cycle.number++;

        _task_queue.async(sequencer_eval_tag,
                          [&]() { _sequencer_eval_task(track); });
      }
    }

    sleep_thread(resolution);
  }

  // Flush all off events
  for (auto& track : _tracks) {
    for (auto& event : track.off_events) {
      _global.handler(track.id, event.value, false);
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
