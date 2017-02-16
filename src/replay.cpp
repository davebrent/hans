#include "hans/replay.hpp"
#include <algorithm>
#include <iostream>

using namespace hans;

ReplayRecorder::ReplayRecorder(std::vector<Parameter::Value>& values,
                               Recordings& recordings)
    : m_values(values), m_recordings(recordings) {
  m_recording = false;
}

void ReplayRecorder::start() {
  if (m_recording) {
    return;
  }

  m_recording = true;
  m_recordings.offsets.push_back(m_recordings.values.size());
}

void ReplayRecorder::tick() {
  if (!m_recording) {
    return;
  }

  m_recordings.values.insert(m_recordings.values.end(),
                             std::make_move_iterator(m_values.begin()),
                             std::make_move_iterator(m_values.end()));
}

void ReplayRecorder::stop() {
  if (!m_recording) {
    return;
  }

  auto offset = m_recordings.offsets.at(m_recordings.offsets.size() - 1);
  auto total = m_recordings.values.size();
  auto framesize = m_values.size();

  auto length = (total - offset) / framesize;
  m_recordings.lengths.push_back(length);
  m_recording = false;
}

ReplayPlayer::ReplayPlayer(std::vector<Parameter::Value>& values,
                           const Recordings& recordings)
    : m_values(values), m_recordings(recordings) {
  m_frameno = 0;
  m_recording = 0;
  m_playing = false;
}

void ReplayPlayer::start() {
  m_playing = true;
}

void ReplayPlayer::stop() {
  m_playing = false;
}

void ReplayPlayer::set(size_t frameno) {
  set(m_recording, frameno);
}

void ReplayPlayer::set(size_t recording, size_t frameno) {
  m_recording = recording;
  m_frameno = frameno;
}

void ReplayPlayer::tick() {
  if (!m_playing) {
    return;
  }

  if (m_recording >= m_recordings.offsets.size()) {
    return;
  }

  auto base = m_recordings.offsets.at(m_recording);
  auto framesize = m_values.size();
  auto offset = base + (framesize * m_frameno);

  if (offset + framesize > m_recordings.values.size()) {
    return;
  }

  auto it = m_recordings.values.begin() + offset;
  std::copy(it, it + framesize, m_values.begin());
  m_frameno++;
}
