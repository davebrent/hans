#ifndef HANS_ENGINE_REPLAY_H_
#define HANS_ENGINE_REPLAY_H_

#include <vector>
#include "hans/common/primitives.hpp"

namespace hans {
namespace engine {

class ReplayRecorder {
 public:
  ReplayRecorder(std::vector<Parameter::Value>& values, Recordings& recordings);

  void start();
  void tick();
  void stop();

 private:
  Recordings& m_recordings;
  std::vector<Parameter::Value>& m_values;
  bool m_recording;
};

class ReplayPlayer {
 public:
  ReplayPlayer(std::vector<Parameter::Value>& values,
               const Recordings& recordings);

  void start();
  void tick();
  void stop();
  void set(size_t frameno);
  void set(size_t recording, size_t frameno);

 private:
  std::vector<Parameter::Value>& m_values;
  const Recordings& m_recordings;
  bool m_playing;
  size_t m_recording;
  size_t m_frameno;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_REPLAY_H_
