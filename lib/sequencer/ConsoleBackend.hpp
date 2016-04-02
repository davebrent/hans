#ifndef HANS_SEQUENCER_CONSOLEBACKEND_H_
#define HANS_SEQUENCER_CONSOLEBACKEND_H_

#include "hans/sequencer/Backend.hpp"
#include "hans/sequencer/types.hpp"

namespace hans {
namespace sequencer {

class ConsoleBackend : public virtual hans::sequencer::Backend {
 public:
  void send(const hans::sequencer::time_event& time);
  void send(const hans::sequencer::clock_event& time);
  void send(const hans::sequencer::note_event& note);
  void send(const hans::sequencer::ctrl_event& event);
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_CONSOLEBACKEND_H_
