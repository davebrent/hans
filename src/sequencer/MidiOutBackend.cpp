#include "./MidiOutBackend.hpp"
#include <cmath>
#include <stdexcept>
#include <string>

using namespace hans;

sequencer::MidiOutBackend::MidiOutBackend(const char* midi_device_name) {
  auto requested = std::string(midi_device_name);
  auto connected = false;

  for (unsigned i = 0; i < m_midiout.getPortCount(); ++i) {
    if (m_midiout.getPortName(i).compare(requested) == 0) {
      m_midiout.openPort(i);
      connected = true;
    }
  }

  if (connected != true) {
    throw std::runtime_error("Midi port name not found: " + requested);
  }

  m_message.reserve(3);
  m_message.push_back(0);
  m_message.push_back(0);
  m_message.push_back(0);
}

void sequencer::MidiOutBackend::send(const sequencer::clock_event& clock) {
  switch (clock.status) {
  case sequencer::TICK:
    m_message[0] = 248;
    m_message[1] = 0;
    m_message[2] = 0;
    m_midiout.sendMessage(&m_message);
    break;

  case sequencer::START:
    m_message[0] = 250;
    m_message[1] = 0;
    m_message[2] = 0;
    m_midiout.sendMessage(&m_message);
    m_started = true;
    break;

  case sequencer::CONTINUE:
    m_message[0] = 251;
    m_message[1] = 0;
    m_message[2] = 0;
    m_midiout.sendMessage(&m_message);
    break;

  case sequencer::STOP:
    m_message[0] = 252;
    m_message[1] = 0;
    m_message[2] = 0;
    m_midiout.sendMessage(&m_message);
    m_started = false;
    break;
  }
}

void sequencer::MidiOutBackend::send(const sequencer::note_event& note) {
  if (note.velocity > 0) {
    // Note on
    m_message[0] = 144 + (note.channel - 1);
    m_message[1] = std::round(note.pitch);
    m_message[2] = std::round(note.velocity);
  } else {
    // Note off
    m_message[0] = 128 + (note.channel - 1);
    m_message[1] = std::round(note.pitch);
    m_message[2] = std::round(note.velocity);
  }

  m_midiout.sendMessage(&m_message);
}

void sequencer::MidiOutBackend::send(const sequencer::ctrl_event& event) {
  m_message[0] = 176 + (event.channel - 1);
  m_message[1] = std::round(event.controller);
  m_message[2] = std::round(event.value);
  m_midiout.sendMessage(&m_message);
}
