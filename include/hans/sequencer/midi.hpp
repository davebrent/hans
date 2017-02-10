#ifndef HANS_SEQUENCER_MIDI_H_
#define HANS_SEQUENCER_MIDI_H_

#include <RtMidi.h>
#include <string>

namespace hans {
namespace sequencer {

class MidiOut {
 public:
  struct Device {
    std::string name;
    size_t index;
  };

  MidiOut();
  std::vector<Device> devices;
  void open(size_t index);
  void send(int byte1, int byt2, int byte3);

 private:
  RtMidiOut m_midi_out;
  std::vector<unsigned char> m_message;
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_MIDI_H_
