#ifndef HANS_ENGINE_REPLAY_H_
#define HANS_ENGINE_REPLAY_H_

#include <sstream>
#include <vector>
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class ReplayRecorder {
 public:
  explicit ReplayRecorder(const std::vector<Parameter::Value>& values);
  ~ReplayRecorder();
  void start();
  void update();
  void stop();
  common::Blob to_blob();

 private:
  common::Blob m_blob;
  std::ostringstream m_stream;
  const std::vector<Parameter::Value>& m_values;
  bool m_recording;
};

class ReplayPlayer {
 public:
  ReplayPlayer(EngineData& ng_data, std::vector<Parameter::Value>& values);
  // ReplayPlayer(std::vector<Parameter::Value>& values);
  void reset_with_blob(common::Blob blob);
  void tick();
  void set(uint64_t frameno);

 private:
  uint64_t m_frameno;
  EngineData& m_ng_data;
  std::vector<Parameter::Value>& m_values;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_REPLAY_H_
