#ifndef HANS_ENGINE_REPLAY_H_
#define HANS_ENGINE_REPLAY_H_

#include <sstream>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class ReplayRecorder {
 public:
  explicit ReplayRecorder(const common::ListView<Parameter::Value>& values);
  ~ReplayRecorder();
  void start();
  void update();
  void stop();
  common::DataFile::Blob to_blob();

 private:
  common::DataFile::Blob m_blob;
  std::ostringstream m_stream;
  const common::ListView<Parameter::Value>& m_values;
  bool m_recording;
};

class ReplayPlayer {
 public:
  ReplayPlayer(common::DataFile::Blob blob,
               common::ListView<Parameter::Value>& values);
  ReplayPlayer(common::ListView<Parameter::Value>& values);
  void reset_with_blob(common::DataFile::Blob blob);
  void tick();
  void set(uint64_t frameno);

 private:
  uint64_t m_frameno;
  common::DataFile::Blob m_blob;
  common::ListView<Parameter::Value>& m_values;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_REPLAY_H_
