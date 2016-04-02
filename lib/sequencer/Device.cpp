#include "hans/sequencer/Device.hpp"
#include <algorithm>
#include <iostream>

using namespace hans;

static bool noteoff(const sequencer::note_event& note) {
  return note.duration <= 0;
}

static bool eventoff(const sequencer::ctrl_event& note) {
  return note.duration <= 0;
}

sequencer::Device::Device(sequencer::Backend* backend) : m_backend(backend) {
}

sequencer::Device::~Device() {
  flush();
  delete m_backend;
}

void sequencer::Device::flush() {
  for (auto& note : m_notes) {
    note.velocity = 0;
    m_backend->send(note);
  }

  m_notes.clear();
  m_events.clear();
}

void sequencer::Device::send(sequencer::clock_event& clock) {
  m_backend->send(clock);
}

void sequencer::Device::send(sequencer::time_event& time) {
  m_backend->send(time);
}

void sequencer::Device::send(sequencer::note_event& note) {
  m_backend->send(note);
  m_notes.push_back(note);
}

void sequencer::Device::send(sequencer::ctrl_event& event) {
  event.revision = 0;
  m_events.push_back(event);
}

bool sequencer::Device::pending() {
  return m_notes.size() != 0 || m_events.size() != 0;
}

void sequencer::Device::tick(float delta) {
  for (auto& note : m_notes) {
    note.duration -= delta;
    if (noteoff(note)) {
      note.velocity = 0;
      m_backend->send(note);
    }
  }

  auto notes_it = std::remove_if(m_notes.begin(), m_notes.end(), noteoff);
  m_notes.erase(notes_it, m_notes.end());

  for (auto& event : m_events) {
    if (event.revision == 0) {
      m_backend->send(event);
    } else if (event.duration >= 0) {
      event.value += (event.target - event.value) * (delta / event.duration);
      event.duration -= delta;
      m_backend->send(event);
    }

    event.revision++;
  }

  auto events_it = std::remove_if(m_events.begin(), m_events.end(), eventoff);
  m_events.erase(events_it, m_events.end());
}
