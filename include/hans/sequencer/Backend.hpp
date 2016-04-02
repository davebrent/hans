#ifndef HANS_SEQUENCER_BACKEND_H_
#define HANS_SEQUENCER_BACKEND_H_

#include "hans/sequencer/types.hpp"

namespace hans {
namespace sequencer {

class Backend {
 public:
  virtual ~Backend() {
  }
  virtual void tick(float delta) {
  }
  virtual void send(const hans::sequencer::clock_event& clock) {
  }
  virtual void send(const hans::sequencer::time_event& time) {
  }
  virtual void send(const hans::sequencer::note_event& note) {
  }
  virtual void send(const hans::sequencer::ctrl_event& ctrl) {
  }
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_BACKEND_H_
