#ifndef HANS_ENGINE_PROGRAMRESOURCES_H_
#define HANS_ENGINE_PROGRAMRESOURCES_H_

#include <vector>
#include "hans/audio/AudioBusManager.hpp"
#include "hans/common/Logger.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/object.hpp"
#include "hans/memory/StringManager.hpp"

namespace hans {
namespace engine {

class ProgramResources {
 public:
  hans::common::Logger* logger;
  hans::memory::StringManager* strings;
  hans_config* config;
  hans::audio::AudioBusManager* audio_buses;
  hans::audio::AudioBufferManager* audio_buffers;
  const std::vector<hans_object>* graphics_objects;
  const std::vector<hans_object>* audio_objects;
  const std::vector<hans_parameter>* parameters;
  const std::vector<hans_shader>* shaders;
  const std::vector<hans_fbo>* frame_buffers;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PROGRAMRESOURCES_H_
