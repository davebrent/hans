#ifndef HANS_SEQUENCER_H_
#define HANS_SEQUENCER_H_

#include <atomic>
#include <chrono>
#include <functional>
#include <thread>
#include "hans/primitives.hpp"
#include "hans/tasks.hpp"

namespace hans {
namespace sequencer {

struct Cycle {
  std::atomic<float> duration;
  std::atomic<size_t> number;

  Cycle() {
    duration.store(0);
    number.store(0);
  }

  Cycle(size_t number_) {
    duration.store(2000);
    number.store(number_);
  }

  Cycle(const Cycle& other) {
    duration.store(other.duration.load());
    number.store(other.number.load());
  }
};

struct Event {
  // The cycle the event should be dispatched in
  uint64_t cycle;
  // Start time in milliseconds relative to start of the cycle
  float start;
  // Duration of the event in milliseconds
  float duration;
  // Value of the event, to be passed back to scheme
  size_t value;
};

using EventList = std::vector<Event>;

namespace detail {

using Handler = std::function<void(Track&, size_t, bool)>;

struct GlobalState {
  std::atomic<bool> stop;
  std::atomic<bool> ready;
  detail::Handler handler;
  GlobalState(detail::Handler handler);
};

class DoubleBuffer {
 public:
  enum class Caller { READER, WRITER };
  std::atomic<bool> swap;
  EventList& get(Caller caller);
  DoubleBuffer();
  DoubleBuffer(const DoubleBuffer& other);
  void clear();

 private:
  EventList m_front;
  EventList m_back;
};

class CycleClock {
 public:
  // Elapsed time since start of the cycle (milliseconds)
  float elapsed;
  void start();
  void tick();
  CycleClock();

 private:
  std::chrono::high_resolution_clock::time_point m_start;
};

struct TrackState {
  Cycle cycle;
  CycleClock clock;
  DoubleBuffer buffer;
  EventList future;
  EventList off_events;
  uint64_t dispatched;
  Track primitive;
  TrackState(hans::Track track);
};

} // namespace detail
} // namespace sequencer

class Sequencer {
 public:
  Sequencer(TaskQueue& task_queue, sequencer::detail::Handler handler,
            Sequences& sequences);
  ~Sequencer();
  void set_program(uint32_t program);
  void run_forever();
  bool stop();

 private:
  sequencer::detail::GlobalState _global;
  std::vector<sequencer::detail::TrackState> _tracks;
  TaskQueue& _task_queue;
  Sequences& _sequences;
  std::atomic<uint32_t> _program;
};

} // namespace hans

#endif // HANS_SEQUENCER_H_
