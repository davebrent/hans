#ifndef HANS_ENGINE_OBJECT_H
#define HANS_ENGINE_OBJECT_H

#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/common/Logger.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/config.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/graphics/FrameBufferManager.hpp"
#include "hans/graphics/ShaderManager.hpp"
#include "hans/graphics/gl.h"

typedef struct {
  hans_config* config;
  hans::engine::ParameterManager* parameters;
  hans::engine::RegisterManager* registers;
  hans::common::Logger* logger;
  hans::common::StringManager* strings;
  hans::audio::AudioBufferManager* audio_buffers;
  hans::audio::AudioBusManager* audio_buses;
  hans::graphics::ShaderManager* shaders;
  hans::graphics::FrameBufferManager* frame_buffers;
} hans_object_api;

typedef struct hans_audio_object hans_audio_object;

struct hans_audio_object {
  hans_object_id object_id;
  hans_instance_id instance_id;
  hans_object_resource* resources;
  unsigned num_resources;
  void* data;
  void (*setup)(hans_audio_object* self, hans_object_api* api);
  void (*callback)(hans_audio_object* self, hans_object_api* api);
};

typedef struct hans_graphics_object hans_graphics_object;

struct hans_graphics_object {
  hans_object_id object_id;
  hans_instance_id instance_id;
  hans_object_resource* resources;
  unsigned num_resources;
  void* data;
  void (*setup)(hans_graphics_object* self, hans_object_api* api);
  void (*update)(hans_graphics_object* self, hans_object_api* api);
  void (*draw)(hans_graphics_object* self, hans_object_api* api);
};

#endif // HANS_ENGINE_OBJECT_H
