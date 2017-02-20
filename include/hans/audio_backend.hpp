#ifndef HANS_AUDIOBACKEND_H_
#define HANS_AUDIOBACKEND_H_

#include <functional>
#include "hans/audio_backend_base.hpp"
#include "hans/audio_buses.hpp"
#include "hans/primitives.hpp"

namespace hans {

AudioBackendBase* make_audio_backend(hans::Settings::Audio& settings,
                                     hans::AudioBuses& buses,
                                     std::function<void(void)> callback);

} // namespace hans

#endif // HANS_AUDIOBACKEND_H_
