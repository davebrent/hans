#include "hans/engine/replay.hpp"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace hans;
using namespace hans::engine;

ReplayRecorder::ReplayRecorder(const std::vector<Parameter::Value>& values)
    : m_values(values), m_stream(std::ostringstream::binary) {
  m_blob.size = 0;
  m_blob.data = nullptr;
  m_recording = false;
}

ReplayRecorder::~ReplayRecorder() {
  if (m_blob.data != nullptr) {
    std::free(m_blob.data);
  }
}

void ReplayRecorder::start() {
  m_stream.seekp(0);
  m_recording = true;
}

void ReplayRecorder::update() {
  if (m_recording) {
    auto bytes = sizeof(Parameter::Value) * m_values.size();
    m_stream.write(
        reinterpret_cast<char*>(const_cast<Parameter::Value*>(&m_values[0])),
        bytes);
  }
}

void ReplayRecorder::stop() {
  if (m_blob.data != nullptr) {
    std::free(m_blob.data);
  }

  auto str = m_stream.str();
  m_blob.size = str.size();
  m_blob.data = std::calloc(m_blob.size, sizeof(char));
  std::memcpy(m_blob.data, str.c_str(), m_blob.size);
}

Blob ReplayRecorder::to_blob() {
  return m_blob;
}

ReplayPlayer::ReplayPlayer(EngineData& ng_data,
                           std::vector<Parameter::Value>& values)
    : m_ng_data(ng_data), m_values(values) {
  m_frameno = -1;
}

// ReplayPlayer::ReplayPlayer(std::vector<Parameter::Value>& values)
//     : m_values(values) {
//   m_frameno = -1;
// }

void ReplayPlayer::reset_with_blob(Blob blob) {
  // assert(m_blob.type == DataFile::REPLAY);
  // m_blob.data = blob.data;
  // m_blob.size = blob.size;
  // m_frameno = -1;
}

void ReplayPlayer::tick() {
  set(m_frameno + 1);
}

void ReplayPlayer::set(uint64_t frameno) {
  // if (m_blob.data == nullptr) {
  //   return;
  // }

  // auto bytes = sizeof(Parameter::Value) * m_values.size();
  // auto offset = frameno * bytes;

  // if (offset >= m_blob.size) {
  //   return;
  // }

  // auto dest = &m_values[0];
  // auto src = reinterpret_cast<void*>(static_cast<char*>(m_blob.data) +
  // offset);
  // std::memcpy(dest, src, bytes);
  // m_frameno = frameno;
}
