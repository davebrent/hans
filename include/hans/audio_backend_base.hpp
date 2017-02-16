#ifndef HANS_AUDIOBACKENDBASE_H_
#define HANS_AUDIOBACKENDBASE_H_

#include <functional>
#include "hans/audio_buses.hpp"
#include "hans/primitives.hpp"

namespace hans {

class AudioBackendBase {
 public:
  AudioBackendBase(const AudioBackendBase& other) = delete;
  AudioBackendBase(const Settings& settings, AudioBuses& buses,
                   std::function<void()> callback)
      : m_settings(settings), m_buses(buses), m_callback(callback){};

  virtual bool open() = 0;
  virtual bool start() = 0;
  virtual bool stop() = 0;
  virtual bool close() = 0;

 protected:
  const Settings& m_settings;
  AudioBuses& m_buses;
  std::function<void()> m_callback;
};

} // namespace hans

#endif // HANS_AUDIOBACKENDBASE_H_
