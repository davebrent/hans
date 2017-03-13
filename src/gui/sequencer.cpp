#include "sequencer.hpp"

gui::Sequencer::Sequencer(hans::TaskQueue& queue, hans::Sequences& sequencers,
                          QObject* parent)
    : QThread(parent),
      _sequencer(queue, sequencers,
                 [this](const hans::Track& track, float value, bool state) {
                   emit trackEvent(track, value, state);
                 }) {
}

void gui::Sequencer::run() {
  _sequencer.run_forever();
}

void gui::Sequencer::stop() {
  _sequencer.stop();
}
