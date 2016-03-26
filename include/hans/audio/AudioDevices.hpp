#ifndef HANS_AUDIO_AUDIODEVICES_H_
#define HANS_AUDIO_AUDIODEVICES_H_

#include "hans/common/types.hpp"
#include <vector>
#include <string>

namespace hans {
namespace audio {

class AudioDevices {
 public:
  AudioDevices();
  ~AudioDevices();

  class Iterator : public std::vector<hans_audio_device>::iterator {
   public:
    explicit Iterator(typename std::vector<hans_audio_device>::iterator c);
  };

  Iterator begin();
  Iterator end();

  std::vector<hans_audio_device> get_devices();

 private:
  std::vector<hans_audio_device> m_devices;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIODEVICES_H_
