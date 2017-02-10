#ifndef HANS_SEQUENCER_SEQUENCER_H_
#define HANS_SEQUENCER_SEQUENCER_H_

#include <atomic>
#include <chrono>
#include <functional>
#include <thread>
#include "hans/sequencer/primitives.hpp"

namespace hans {
namespace sequencer {
namespace detail {

using Callback = std::function<EventList(Cycle&)>;
using Handler = std::function<void(size_t, size_t, bool)>;

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

struct Track {
  uint64_t id;
  Cycle cycle;
  CycleClock clock;
  DoubleBuffer buffer;
  detail::Callback producer;
  EventList future;
  EventList off_events;
  uint64_t next_cycle;
  uint64_t dispatched;
  Track(uint64_t _id, detail::Callback callback);
};

} // namespace detail

class Sequencer {
 public:
  Sequencer(detail::Handler handler);
  ~Sequencer();
  size_t add_track(detail::Callback track);
  bool run_forever();
  bool stop();
  bool join();

 private:
  detail::GlobalState global;
  std::vector<detail::Track> tracks;
  std::thread* m_producer = nullptr;
  std::thread* m_consumer = nullptr;
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_SEQUENCER_H_
