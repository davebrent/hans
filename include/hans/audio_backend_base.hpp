#ifndef HANS_AUDIOBACKENDBASE_H_
#define HANS_AUDIOBACKENDBASE_H_

#include <functional>
#include "hans/audio_buses.hpp"
#include "hans/primitives.hpp"

namespace hans {

class AudioBackendBase {
 public:
  AudioBackendBase(const AudioBackendBase& other) = delete;
  AudioBackendBase(const Settings::Audio& settings, AudioBuses& buses,
                   std::function<void()> callback)
      : _settings(settings), _buses(buses), _callback(callback){};

  virtual bool open() = 0;
  virtual bool start() = 0;
  virtual bool stop() = 0;
  virtual bool close() = 0;

 protected:
  const Settings::Audio& _settings;
  AudioBuses& _buses;
  std::function<void()> _callback;
};

} // namespace hans

#endif // HANS_AUDIOBACKENDBASE_H_
