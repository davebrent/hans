#ifndef HANS_AUDIO_AUDIODEVICES_H_
#define HANS_AUDIO_AUDIODEVICES_H_

#include <string>
#include <vector>
#include "hans/audio/types.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

class AudioDevices {
 public:
  AudioDevices();
  ~AudioDevices();

  class Iterator : public std::vector<Device>::iterator {
   public:
    explicit Iterator(typename std::vector<Device>::iterator c);
  };

  Iterator begin();
  Iterator end();

  std::vector<Device> get_devices();

 private:
  std::vector<Device> m_devices;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIODEVICES_H_
