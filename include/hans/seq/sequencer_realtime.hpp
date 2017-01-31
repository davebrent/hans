#ifndef HANS_SEQ_SEQUENCER_REALTIME_H_
#define HANS_SEQ_SEQUENCER_REALTIME_H_

#include <atomic>
#include <chrono>
#include <thread>
#include "hans/seq/sequencer_base.hpp"

namespace hans {
namespace seq {
namespace detail {

struct GlobalState {
  std::atomic<bool> stop;
  std::atomic<bool> ready;

  SequencerBase::Handler handler;
  GlobalState(SequencerBase::Handler handler);
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

  CycleClock(float cycle_duration);
  void start();
  void tick();

 private:
  std::chrono::high_resolution_clock::time_point m_start;
  float m_cycle_duration;
};

struct Track {
  uint64_t id;
  Cycle cycle;
  CycleClock clock;
  DoubleBuffer buffer;
  SequencerBase::Callback producer;
  EventList future;
  EventList off_events;
  uint64_t next_cycle;
  uint64_t dispatched;

  Track(uint64_t _id, float duration, SequencerBase::Callback callback);
};

} // namespace detail

class SequencerRealtime : public SequencerBase {
 public:
  detail::GlobalState global;
  std::vector<detail::Track> tracks;

  SequencerRealtime(SequencerBase::Handler handler);
  ~SequencerRealtime();
  virtual size_t add_track(float duration, Callback track) override;
  virtual bool start(SequencerBase::Processor producer,
                     SequencerBase::Processor consumer) override;
  virtual bool stop() override;

 private:
  std::thread* m_producer;
  std::thread* m_consumer;
};

} // namespace seq
} // namespace hans

#endif // HANS_SEQ_SEQUENCER_REALTIME_H_
