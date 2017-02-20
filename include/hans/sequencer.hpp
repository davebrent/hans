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

class ForegroundState {
 public:
  Track& track;
  float duration;
  float elapsed;
  size_t position;
  EventList on;
  EventList off;

  ForegroundState(Track& track);
  bool tick();
  void reset();

 private:
  std::chrono::high_resolution_clock::time_point _started;
};

class BackgroundState {
 public:
  BackgroundState();
  void synchronize(ForegroundState& fg);
  void schedule(size_t cycle, Track& track);

 private:
  std::atomic<bool> _evaling;
  std::mutex _overrun;
  std::mutex _mutex;
  EventList _events;
  float _duration;
};

} // namespace sequencer

class Sequencer {
 public:
  using Handler = std::function<void(const Track&, size_t, bool)>;
  enum Mode { RELOAD = 0, INIT = 1, RUN = 2 };

  Sequencer(TaskQueue& task_queue, Sequences& sequences, Handler handler);
  ~Sequencer();

  void reload(Sequences& sequences);
  bool stop();
  void set_program(uint32_t program);
  void run_forever();

 private:
  void stop_state();
  void reload_state();
  void init_state();
  void run_state();
  void process_on_events(sequencer::ForegroundState& state);
  void process_off_events(sequencer::ForegroundState& state, float delta);

  TaskQueue& _task_queue;
  Sequences& _sequences;
  Handler _handler;
  size_t _program;
  std::vector<sequencer::BackgroundState> _bg_states;
  std::vector<sequencer::ForegroundState> _fg_states;
  std::vector<size_t> _cycles;
  std::atomic<bool> _stop;
  std::atomic<int> _mode;
  std::mutex _mutex;
};

} // namespace hans

#endif // HANS_SEQUENCER_H_
