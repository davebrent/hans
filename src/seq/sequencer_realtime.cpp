#include "hans/seq/sequencer_realtime.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
#include <functional>
#include <new>
#include "hans/seq/primitives.hpp"

using namespace hans::seq;
using namespace hans::seq::detail;
using namespace std::chrono;

static void sleep_thread(float ms) {
  std::this_thread::sleep_for(microseconds(static_cast<long>(ms * 1000)));
}

GlobalState::GlobalState(SequencerBase::Handler handler_) : handler(handler_) {
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

CycleClock::CycleClock(float cycle_duration) {
  m_cycle_duration = cycle_duration;
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

Track::Track(uint64_t _id, float duration, SequencerBase::Callback callback)
    : id(_id), cycle(duration, 0), clock(duration), producer(callback) {
  dispatched = 0;
  next_cycle = 0;
}

static bool sort_by_cycle(const Event& a, const Event& b) {
  return a.cycle < b.cycle;
}

static bool sort_by_time(const Event& a, const Event& b) {
  return a.start < b.start;
}

static bool sort_by_duration(const Event& a, const Event& b) {
  return a.duration < b.duration;
}

static bool parse_events(EventList& output, EventList& list) {
  for (auto& event : list) {
    output.push_back(event);
  }
  return true;
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

  std::sort(future.begin(), future.end(), sort_by_cycle);

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

static bool generate_track_events(EventList& events, Track& track,
                                  uint64_t current_cycle) {
  // Generate and parse a list of events from scheme
  auto value = track.producer(track.cycle);
  if (!parse_events(events, value)) {
    return false;
  }

  // Sort and filter events
  auto& buffer = track.buffer.get(DoubleBuffer::Caller::WRITER);
  get_current_events(buffer, track.future, events, track.cycle);
  add_future_events(buffer, track.future, track.cycle);
  std::sort(buffer.begin(), buffer.end(), sort_by_time);
  events.clear();

  // Swap the tracks buffers
  auto side = track.buffer.swap.load();
  track.buffer.swap.store(!side);
  return true;
}

// Produce events to be dispatched in the next cycle.
// Watches the shared cycle number for triggering processing
static void produce_events(GlobalState& global, std::vector<Track>& tracks) {
  auto events = EventList();

  // Initial events
  for (auto& track : tracks) {
    track.next_cycle = track.cycle.number.load() + 1;
    if (!generate_track_events(events, track, 0)) {
      global.stop.store(true);
      return;
    }
  }

  global.ready.store(true);

  while (!global.stop.load()) {
    sleep_thread(1);

    for (auto& track : tracks) {
      auto cycle = track.cycle.number.load();
      if (cycle == track.next_cycle) {
        track.next_cycle += 1;
        if (!generate_track_events(events, track, cycle)) {
          global.stop.store(true);
          return;
        }
      }
    }
  }
}

// Dispatch all start-events, adding them to the end-event list if needed
static void process_on_events(EventList& on_events, Track& track,
                              SequencerBase::Handler& handler) {
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
static void process_off_events(Track& track, float delta,
                               SequencerBase::Handler& handler) {
  auto removed = 0;

  std::sort(track.off_events.begin(), track.off_events.end(), sort_by_duration);

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

// Consume current cycles events, dispatching them back to scheme
static void consume_events(GlobalState& global, std::vector<Track>& tracks) {
  auto resolution = 0.32; /* milliseconds */

  // Spin until all tracks are ready
  while (true) {
    sleep_thread(1);

    if (global.stop.load()) {
      return;
    }

    if (global.ready.load()) {
      break;
    }
  }

  // Start all track clocks
  for (auto& track : tracks) {
    track.clock.start();
  }

  // Spin all tracks independently
  while (!global.stop.load()) {
    for (auto& track : tracks) {
      track.clock.tick();

      auto& on_events = track.buffer.get(DoubleBuffer::Caller::READER);
      process_on_events(on_events, track, global.handler);
      process_off_events(track, resolution, global.handler);

      if (track.clock.elapsed >= track.cycle.duration.load()) {
        on_events.clear();
        track.dispatched = 0;
        track.clock.start();
        track.cycle.number++;
      }
    }

    sleep_thread(resolution);
  }

  // Flush all off events
  for (auto& track : tracks) {
    for (auto& event : track.off_events) {
      global.handler(track.id, event.value, false);
    }
  }
}

SequencerRealtime::SequencerRealtime(SequencerBase::Handler handler)
    : global(handler) {
}

SequencerRealtime::~SequencerRealtime() {
  stop();
}

size_t SequencerRealtime::add_track(float duration,
                                    SequencerBase::Callback callback) {
  auto id = tracks.size();
  tracks.push_back(std::move(Track(id, duration, callback)));
  return id;
}

bool SequencerRealtime::start(SequencerBase::Processor producer_processor,
                              SequencerBase::Processor consumer_processor) {
  if (m_producer != nullptr || m_consumer != nullptr) {
    return false;
  }

  for (auto& track : tracks) {
    track.cycle.number.store(0);
    track.buffer.swap.store(false);
    track.dispatched = 0;
  }

  m_producer = new std::thread(producer_processor, this);
  m_consumer = new std::thread(consumer_processor, this);
  return true;
}

bool SequencerRealtime::stop() {
  global.stop.store(true);

  m_producer->join();
  m_consumer->join();

  delete m_producer;
  delete m_consumer;

  m_producer = nullptr;
  m_consumer = nullptr;

  for (auto& track : tracks) {
    track.buffer.clear();
  }

  return true;
}
