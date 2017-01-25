#ifndef HANS_ENGINE_AUDIOBACKENDIMPL_H_
#define HANS_ENGINE_AUDIOBACKENDIMPL_H_

#include <functional>
#include "hans/common/primitives.hpp"
#include "hans/engine/AudioBusManager.hpp"

namespace hans {
namespace engine {

class AudioBackendImpl {
 public:
  AudioBackendImpl(const AudioBackendImpl& other) = delete;
  AudioBackendImpl(const Settings& settings, AudioBusManager& buses,
                   std::function<void()> callback)
      : m_settings(settings), m_buses(buses), m_callback(callback){};

  virtual bool open() = 0;
  virtual bool start() = 0;
  virtual bool stop() = 0;
  virtual bool close() = 0;

 protected:
  const Settings& m_settings;
  AudioBusManager& m_buses;
  std::function<void()> m_callback;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBACKENDIMPL_H_
