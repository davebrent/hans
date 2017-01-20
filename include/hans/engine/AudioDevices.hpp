#ifndef HANS_ENGINE_AUDIODEVICES_H_
#define HANS_ENGINE_AUDIODEVICES_H_

#include <vector>
#include "hans/common/primitives.hpp"

namespace hans {
namespace engine {

class AudioDevices {
 public:
  AudioDevices();
  ~AudioDevices();

  class Iterator : public std::vector<audio::Device>::iterator {
   public:
    explicit Iterator(typename std::vector<audio::Device>::iterator c);
  };

  Iterator begin();
  Iterator end();

  std::vector<audio::Device> get_devices();

 private:
  std::vector<audio::Device> m_devices;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIODEVICES_H_
