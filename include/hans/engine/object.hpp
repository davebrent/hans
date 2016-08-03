#ifndef HANS_ENGINE_OBJECT_H
#define HANS_ENGINE_OBJECT_H

#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ModulationManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/graphics/FrameBufferManager.hpp"
#include "hans/graphics/ShaderManager.hpp"
#include "hans/graphics/gl.h"

typedef struct {
  hans_config* config;
  hans::engine::ParameterManager* parameters;
  hans::engine::RegisterManager* registers;
  hans::engine::ModulationManager* modulators;
  hans::common::StringManager* strings;
  hans::audio::AudioBufferManager* audio_buffers;
  hans::audio::AudioBusManager* audio_buses;
  hans::audio::RingBufferManager* ring_buffers;
  hans::graphics::ShaderManager* shaders;
  hans::graphics::FrameBufferManager* fbos;
} hans_object_api;

namespace hans {

class Object {
 public:
  Object(hans_instance_id _id) {
    id = _id;
  }

  virtual ~Object() {
  }
  virtual void create(ObjectPatcher& patcher) = 0;
  virtual void setup(hans_object_api& api) = 0;

 protected:
  hans_instance_id id;
};

class AudioObject : protected Object {
 public:
  using Object::Object;
  virtual void callback(hans_object_api& api) = 0;
};

class GraphicsObject : protected Object {
 public:
  using Object::Object;
  virtual void update(hans_object_api& api) = 0;
  virtual void draw(hans_object_api& api) = 0;
};
}

#endif // HANS_ENGINE_OBJECT_H
