#include "hans/sequencer.hpp"
#include <algorithm>
#include <thread>
#include "hans/interpreter.hpp"

using namespace hans;
using namespace hans::sequencer;
using namespace std::chrono;

ForegroundState::ForegroundState(Track& track) : track(track) {
  duration = 0;
  elapsed = 0;
  position = 0;
}

bool ForegroundState::tick() {
  auto now = high_resolution_clock::now();
  elapsed = duration_cast<microseconds>(now - _started).count() / 1000.f;
  return elapsed < duration;
}

void ForegroundState::reset() {
  duration = 0;
  elapsed = 0;
  position = 0;
  on.clear();
  off.clear();
  _started = high_resolution_clock::now();
}

BackgroundState::BackgroundState() : _duration(0) {
  _evaling.store(false);
}

void BackgroundState::synchronize(ForegroundState& fg) {
  std::lock_guard<std::mutex> lck(_mutex);
  fg.duration = _duration;
  fg.on = _events;
}

void BackgroundState::schedule(size_t number, Track& track) {
  // Drop in the case of a backlog
  // XXX: Standard mentions spurious failures?
  bool expected = false;
  if (!_evaling.compare_exchange_strong(expected, true)) {
    return;
  }

  Cycle cycle(number);
  Interpreter itp(cycle, track.instructions);
  interpret(itp);
  auto value = itp.dstack.pop();
  auto events = interpreter::to_events(itp.cycle, value.tree);
  std::sort(events.begin(), events.end(),
            [](const Event& a, const Event& b) { return a.start < b.start; });

  {
    std::lock_guard<std::mutex> lck(_mutex);
    _duration = cycle.duration;
    _events = events;
  }

  _evaling.store(false);
}

void Sequencer::process_on_events(ForegroundState& state) {
  auto length = state.on.size();
  while (state.position < length) {
    auto& event = state.on.at(state.position);
    if (state.elapsed < event.start) {
      break;
    }

    _handler(state.track, event.value, true);
    state.position++;

    if (event.duration != 0) {
      state.off.push_back(event);
    }
  }
}

static bool sort_by_duration(const Event& a, const Event& b) {
  return a.duration < b.duration;
}

void Sequencer::process_off_events(ForegroundState& state, float delta) {
  auto removed = 0;
  std::sort(state.off.begin(), state.off.end(), sort_by_duration);

  for (auto& event : state.off) {
    if (event.duration <= 0) {
      _handler(state.track, event.value, false);
      removed++;
    } else {
      event.duration -= delta;
    }
  }

  if (removed) {
    state.off.erase(state.off.begin(), state.off.begin() + removed);
  }
}

Sequencer::Sequencer(TaskQueue& task_queue, Sequences& sequences,
                     Handler handler)
    : _task_queue(task_queue),
      _sequences(sequences),
      _handler(handler),
      _program(0),
      _bg_states(_sequences.tracks.size()) {
  _program = 0;
  _stop.store(false);
  _mode.store(RELOAD);
}

Sequencer::~Sequencer() {
  stop_state();
}

void Sequencer::set_program(uint32_t program) {
  std::lock_guard<std::mutex> lck(_mutex);
  if (program < _sequences.ranges.size()) {
    _program = program;
  }
}

void Sequencer::reload(Sequences& sequences) {
  {
    std::lock_guard<std::mutex> lck(_mutex);
    _sequences = sequences;
  }

  _mode.store(RELOAD);
}

bool Sequencer::stop() {
  _stop.store(true);
  return true;
}

void Sequencer::stop_state() {
  for (auto& fg : _fg_states) {
    for (auto& event : fg.off) {
      _handler(fg.track, event.value, false);
    }
  }
}

void Sequencer::reload_state() {
  stop_state();

  _program = 0;
  _fg_states.clear();
  _cycles.clear();

  _fg_states.reserve(_sequences.tracks.size());
  _cycles.reserve(_sequences.tracks.size());
  _bg_states = std::vector<BackgroundState>(_sequences.tracks.size());

  for (auto& track : _sequences.tracks) {
    _fg_states.push_back(ForegroundState(track));
    _cycles.push_back(0);
  }

  init_state();
  run_state();
}

void Sequencer::init_state() {
  for (auto i = 0; i < _bg_states.size(); ++i) {
    auto& bg = _bg_states.at(i);
    auto& fg = _fg_states.at(i);

    bg.schedule(0, fg.track);
    bg.synchronize(fg);

    _task_queue.async(TaskQueue::SEQUENCER_EVAL,
                      [&bg, &fg] { bg.schedule(1, fg.track); });
  }

  _mode.store(RUN);
}

void Sequencer::run_state() {
  auto res = 0.12; /* milliseconds */
  Range range;

  {
    std::lock_guard<std::mutex> lck(_mutex);
    if (_program >= _sequences.ranges.size()) {
      return;
    }
    range = _sequences.ranges.at(_program);
  }

  for (auto i = range.start; i < range.end; ++i) {
    auto& fg = _fg_states.at(i);
    process_on_events(fg);
    process_off_events(fg, res);
    if (fg.tick()) {
      continue;
    }

    auto cycle = ++_cycles.at(i);
    fg.reset();
    auto& bg = _bg_states.at(i);
    bg.synchronize(fg);
    _task_queue.async(TaskQueue::SEQUENCER_EVAL,
                      [cycle, &bg, &fg]() { bg.schedule(cycle, fg.track); });
  }

  std::this_thread::sleep_for(microseconds(static_cast<long>(res * 1000)));
}

void Sequencer::run_forever() {
  while (!_stop.load()) {
    switch (_mode.load()) {
    case RELOAD:
      reload_state();
      break;
    case INIT:
      init_state();
      break;
    case RUN:
      run_state();
      break;
    }
  }

  stop_state();
}
