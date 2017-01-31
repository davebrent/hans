#include "hans/seq/midi.hpp"

using namespace hans::seq;

MidiOut::MidiOut() : m_message(3, 0) {
  auto len = m_midi_out.getPortCount();
  for (auto i = 0; i < len; ++i) {
    Device device;
    device.name = m_midi_out.getPortName(i);
    device.index = i;
    devices.push_back(device);
  }
}

void MidiOut::open(size_t index) {
  m_midi_out.openPort(index);
}

void MidiOut::send(int byte1, int byte2, int byte3) {
  m_message[0] = byte1;
  m_message[1] = byte2;
  m_message[2] = byte3;
  m_midi_out.sendMessage(&m_message);
}
