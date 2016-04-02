#ifndef HANS_SEQUENCER_MIDIOUTBACKEND_H_
#define HANS_SEQUENCER_MIDIOUTBACKEND_H_

#include <RtMidi.h>
#include <vector>
#include "hans/sequencer/Backend.hpp"
#include "hans/sequencer/types.hpp"

namespace hans {
namespace sequencer {

class MidiOutBackend : public virtual hans::sequencer::Backend {
 public:
  MidiOutBackend(const char* midi_device);
  void send(const hans::sequencer::clock_event& clock);
  void send(const hans::sequencer::note_event& note);
  void send(const hans::sequencer::ctrl_event& event);

 private:
  bool m_started;
  RtMidiOut m_midiout;
  std::vector<unsigned char> m_message;
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_MIDIOUTBACKEND_H_
