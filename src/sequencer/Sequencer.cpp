#include "./Sequencer.hpp"
#include <libguile.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <functional>
#include <new>
#include <thread>
#include "./types.hpp"

using namespace hans;
using namespace hans::sequencer;
using namespace std::chrono;

void sleep_thread(float ms) {
  std::this_thread::sleep_for(microseconds(static_cast<long>(ms * 1000)));
}

class CycleClock {
 public:
  // Elapsed time since start of the cycle (milliseconds)
  float elapsed;

  CycleClock(float cycle_duration) {
    m_cycle_duration = cycle_duration;
    m_start = high_resolution_clock::now();
    elapsed = 0;
  }

  void start() {
    m_start = high_resolution_clock::now();
    elapsed = 0;
  }

  void tick() {
    auto now = high_resolution_clock::now();
    elapsed = duration_cast<microseconds>(now - m_start).count() / 1000.f;
  }

 private:
  high_resolution_clock::time_point m_start;
  float m_cycle_duration;
};

class DoubleBuffer {
 public:
  enum class Caller { READER, WRITER };
  std::atomic<bool> swap;

  EventList& get(Caller caller) {
    bool value = swap.load();
    switch (caller) {
    case Caller::READER:
      return (value) ? m_back : m_front;
    case Caller::WRITER:
      return (value) ? m_front : m_back;
    }
  }

  DoubleBuffer() {
    swap.store(false);
  }

  DoubleBuffer(const DoubleBuffer& other) {
    swap.store(other.swap.load());
    m_front = other.m_front;
    m_back = other.m_back;
  }

  void clear() {
    m_front.clear();
    m_back.clear();
  }

 private:
  EventList m_front;
  EventList m_back;
};

static SCM make_cycle(SCM start, SCM duration, SCM number) {
  return scm_list_3(start, duration, number);
}

static SCM cycle_start(SCM cycle) {
  return scm_list_ref(cycle, scm_from_int(0));
}

static SCM cycle_duration(SCM cycle) {
  return scm_list_ref(cycle, scm_from_int(1));
}

static SCM cycle_number(SCM cycle) {
  return scm_list_ref(cycle, scm_from_int(2));
}

struct Cycle {
  std::atomic<uint64_t> number;
  std::atomic<float> duration;

  SCM to_scm() {
    return make_cycle(scm_from_int(0), scm_from_double(duration.load()),
                      scm_from_uint64(number.load()));
  }

  Cycle(float ms) {
    duration.store(ms);
    number.store(0);
  }

  Cycle(const Cycle& other) {
    number.store(other.number.load());
    duration.store(other.duration.load());
  }
};

struct GlobalState {
  std::atomic<bool> stop;
  std::atomic<bool> ready;
  SCM handler;

  GlobalState() {
    stop.store(false);
    ready.store(false);
    handler = SCM_BOOL_F;
  }

  GlobalState(const GlobalState& other) {
    stop.store(other.stop.load());
    ready.store(other.ready.load());
    handler = other.handler;
  }
};

struct Track {
  Cycle cycle;
  CycleClock clock;
  DoubleBuffer buffer;
  SCM producer;
  EventList future;
  EventList off_events;
  uint64_t next_cycle;
  uint64_t dispatched;

  Track(float duration, SCM _producer)
      : cycle(duration), clock(duration), producer(_producer) {
    dispatched = 0;
    next_cycle = 0;
  }
};

static bool sort_by_cycle(const Event& a, const Event& b) {
  return a.cycle < b.cycle;
}

static bool sort_by_time(const Event& a, const Event& b) {
  return a.start < b.start;
}

static bool sort_by_duration(const Event& a, const Event& b) {
  return a.duration < b.duration;
}

static bool parse_events(EventList& output, SCM list) {
  auto zero = scm_from_int(0);
  auto one = scm_from_int(1);
  auto two = scm_from_int(2);

  auto length = scm_to_int(scm_length(list));
  for (auto i = 0; i < length; ++i) {
    auto item = scm_list_ref(list, scm_from_int(i));
    if (scm_is_false(scm_list_p(item))) {
      return false;
    }

    auto event = Event();
    switch (scm_to_int(scm_length(item))) {
    case 2:
      event.start = scm_to_double(scm_list_ref(item, zero));
      event.duration = 0;
      event.value = scm_list_ref(item, one);
      break;
    case 3:
      event.start = scm_to_double(scm_list_ref(item, zero));
      event.duration = scm_to_double(scm_list_ref(item, one));
      event.value = scm_list_ref(item, two);
      break;
    default:
      return false;
    }

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
  auto value = scm_call_1(track.producer, track.cycle.to_scm());
  if (scm_is_false(scm_list_p(value)) || !parse_events(events, value)) {
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
                              SCM procedure) {
  auto length = on_events.size();

  while (track.dispatched < length) {
    auto& event = on_events.at(track.dispatched);
    if (track.clock.elapsed < event.start) {
      break;
    }

    scm_call_2(procedure, event.value, SCM_BOOL_T);
    if (event.duration != 0) {
      track.off_events.push_back(event);
    }

    track.dispatched++;
  }
}

// Tick and dispatch end-events
static void process_off_events(Track& track, float delta, SCM procedure) {
  auto removed = 0;

  std::sort(track.off_events.begin(), track.off_events.end(), sort_by_duration);

  for (auto& event : track.off_events) {
    if (event.duration <= 0) {
      scm_call_2(procedure, event.value, SCM_BOOL_F);
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
      scm_call_2(global.handler, event.value, SCM_BOOL_F);
    }
  }
}

class Sequencer {
 public:
  typedef std::function<void(Sequencer*)> Processor;

  GlobalState global;
  std::vector<Track> tracks;

  ~Sequencer() {
    stop();
  }

  int add_track(float duration, SCM procedure) {
    tracks.push_back(Track(duration, procedure));
    return tracks.size() - 1;
  }

  bool start(Processor producer_processor, Processor consumer_processor) {
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

  bool stop() {
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

 private:
  std::thread* m_producer;
  std::thread* m_consumer;
};

static void* consumer_inner(void* data) {
  auto sequencer = static_cast<Sequencer*>(data);
  consume_events(sequencer->global, sequencer->tracks);
  return nullptr;
}

static void* producer_inner(void* data) {
  auto sequencer = static_cast<Sequencer*>(data);
  produce_events(sequencer->global, sequencer->tracks);
  return nullptr;
}

static void consumer_outer(Sequencer* sequencer) {
  scm_with_guile(consumer_inner, sequencer);
}

static void producer_outer(Sequencer* sequencer) {
  scm_with_guile(producer_inner, sequencer);
}

static scm_t_bits SequencerTag;

static Sequencer* scm_to_sequencer(SCM sequencer) {
  scm_assert_smob_type(SequencerTag, sequencer);
  return reinterpret_cast<Sequencer*>(SCM_SMOB_DATA(sequencer));
}

static SCM make_sequencer() {
  auto place = scm_gc_malloc(sizeof(Sequencer), "sequencer");
  auto sequencer = new (place) Sequencer();
  return scm_new_smob(SequencerTag, (scm_t_bits)sequencer);
}

static SCM sequencer_track(SCM sequencer, SCM duration, SCM procedure) {
  auto instance = scm_to_sequencer(sequencer);
  auto result = instance->add_track(scm_to_double(duration), procedure);
  scm_gc_protect_object(procedure);
  return scm_from_int(result);
}

static SCM sequencer_handler(SCM sequencer, SCM handler) {
  auto instance = scm_to_sequencer(sequencer);
  if (scm_is_false(instance->global.handler)) {
    instance->global.handler = handler;
    scm_gc_protect_object(instance->global.handler);
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM sequencer_start(SCM sequencer) {
  auto instance = scm_to_sequencer(sequencer);
  return scm_from_bool(instance->start(producer_outer, consumer_outer));
}

static SCM sequencer_stop(SCM sequencer) {
  auto instance = scm_to_sequencer(sequencer);
  return scm_from_bool(instance->stop());
}

static SCM sequencer_destroy(SCM sequencer) {
  auto instance = scm_to_sequencer(sequencer);

  if (!scm_is_false(instance->global.handler)) {
    scm_gc_unprotect_object(instance->global.handler);
  }

  for (const auto& track : instance->tracks) {
    scm_gc_unprotect_object(track.producer);
  }

  scm_gc_free(instance, sizeof(Sequencer), "sequencer");
  return SCM_BOOL_T;
}

void hans::sequencer::init_sequencer_module() {
  scm_c_define_gsubr("make-sequencer", 0, 0, 0, (scm_t_subr)make_sequencer);
  SequencerTag = scm_make_smob_type("sequencer", sizeof(Sequencer));
  scm_c_define_gsubr("sequencer-track", 3, 0, 0, (scm_t_subr)sequencer_track);
  scm_c_define_gsubr("sequencer-handler", 2, 0, 0,
                     (scm_t_subr)sequencer_handler);
  scm_c_define_gsubr("sequencer-start", 1, 0, 0, (scm_t_subr)sequencer_start);
  scm_c_define_gsubr("sequencer-stop", 1, 0, 0, (scm_t_subr)sequencer_stop);
  scm_c_define_gsubr("sequencer-destroy", 1, 0, 0,
                     (scm_t_subr)sequencer_destroy);

  scm_c_define_gsubr("make-cycle", 3, 0, 0, (scm_t_subr)make_cycle);
  scm_c_define_gsubr("cycle-number", 1, 0, 0, (scm_t_subr)cycle_number);
  scm_c_define_gsubr("cycle-start", 1, 0, 0, (scm_t_subr)cycle_start);
  scm_c_define_gsubr("cycle-duration", 1, 0, 0, (scm_t_subr)cycle_duration);
}
