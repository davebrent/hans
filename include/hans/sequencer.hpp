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
  TaskQueue& _task_queue;
  sequencer::detail::GlobalState _global;
  Sequences& _sequences;
  std::vector<sequencer::detail::TrackState> _tracks;
  std::atomic<uint32_t> _program;
};

} // namespace hans

#endif // HANS_SEQUENCER_H_
