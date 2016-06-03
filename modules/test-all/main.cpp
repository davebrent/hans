#include <math.h>
#include <cassert>
#include <exception>
#include <sstream>
#include "./test.all_generated.h"
#include "hans/engine/object.hpp"

using namespace hans;

typedef struct {
  hans_parameter_handle hans_test_graphics_1;
  hans_parameter_handle hans_test_graphics_2;
} hans_test_gfx_data;

typedef struct {
  hans_parameter_handle hans_test_audio_1;
  hans_parameter_handle hans_test_audio_2;
} hans_test_audio_data;

void hans_test_graphics_setup(hans_graphics_object* self,
                              hans_object_api* api) {
  auto data = static_cast<hans_test_gfx_data*>(self->data);

  for (int i = 0; i < self->num_resources; ++i) {
    auto resource = &self->resources[i];
    switch (resource->type) {
    case HANS_PARAMETER: {
      switch (resource->name) {
      case HANS_TEST_PARAM_GFX_1:
        data->hans_test_graphics_1 = self->resources[i].parameter;
        api->logger->log(
            common::Logger::INFO,
            "HANS_GRAPHICS_TEST PARAMETER name=hans_test_graphics_1");
        break;
      case HANS_TEST_PARAM_GFX_2:
        data->hans_test_graphics_2 = self->resources[i].parameter;
        api->logger->log(
            common::Logger::INFO,
            "HANS_GRAPHICS_TEST PARAMETER name=hans_test_graphics_2");
        break;
      }
      break;
    }
    case HANS_INLET:
      api->logger->log(common::Logger::INFO, "HANS_GRAPHICS_TEST INLET");
      break;
    case HANS_OUTLET:
      api->logger->log(common::Logger::INFO, "HANS_GRAPHICS_TEST OUTLET");
      break;
    case HANS_SHADER:
      api->logger->log(common::Logger::INFO, "HANS_GRAPHICS_TEST SHADER");
      break;
    case HANS_FRAME_BUFFER:
    case HANS_AUDIO_BUFFER:
      throw std::runtime_error("Unknown resource recieved");
      break;
    }
  }
}

void hans_test_graphics_update(hans_graphics_object* self,
                               hans_object_api* api) {
  auto data = static_cast<hans_test_gfx_data*>(self->data);
  assert(api->parameters->get(data->hans_test_graphics_1, 0) == 1);
  assert(api->parameters->get(data->hans_test_graphics_2, 0) == 3);
  assert(api->parameters->get(data->hans_test_graphics_2, 1) == 2);
}

void hans_test_graphics_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<hans_test_gfx_data*>(self->data);
  assert(api->parameters->get(data->hans_test_graphics_1, 0) == 1);
  assert(api->parameters->get(data->hans_test_graphics_2, 0) == 3);
  assert(api->parameters->get(data->hans_test_graphics_2, 1) == 2);
}

void hans_test_graphics_new(hans_constructor_api* api, void* buff,
                            size_t size) {
  auto arguments = api->get_args(api);
  for (int i = 0; i < arguments.length; ++i) {
    if (arguments.data[i].type == HANS_NUMBER) {
      api->request_resource(api, HANS_INLET, arguments.data[i].number);
      break;
    }
  }
}

void hans_test_graphics_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = hans_test_graphics_setup;
  object->update = hans_test_graphics_update;
  object->draw = hans_test_graphics_draw;
}

void hans_test_audio_setup(hans_audio_object* self, hans_object_api* api) {
  auto data = static_cast<hans_test_audio_data*>(self->data);

  for (int i = 0; i < self->num_resources; ++i) {
    auto resource = &self->resources[i];
    switch (resource->type) {
    case HANS_PARAMETER: {
      switch (resource->name) {
      case HANS_TEST_PARAM_AUDIO_1:
        data->hans_test_audio_1 = self->resources[i].parameter;
        api->logger->log(common::Logger::INFO,
                         "HANS_AUDIO_TEST PARAMETER name=hans_test_audio_1");
        break;

      case HANS_TEST_PARAM_AUDIO_2:
        data->hans_test_audio_2 = self->resources[i].parameter;
        api->logger->log(common::Logger::INFO,
                         "HANS_AUDIO_TEST PARAMETER name=hans_test_audio_2");
        break;
      }
      break;
    }

    case HANS_INLET:
      api->logger->log(common::Logger::INFO, "HANS_AUDIO_TEST INLET");
      break;

    case HANS_OUTLET:
      api->logger->log(common::Logger::INFO, "HANS_AUDIO_TEST OUTLET");
      break;

    case HANS_AUDIO_BUFFER:
      api->logger->log(common::Logger::INFO, "HANS_AUDIO_TEST AUDIO_BUFFER");
      break;

    case HANS_FRAME_BUFFER:
    case HANS_SHADER:
      throw std::runtime_error("Unknown resource recieved");
      break;
    }
  }
}

void hans_test_audio_callback(hans_audio_object* self, hans_object_api* api) {
  // auto data = static_cast<hans_test_audio_data *>(self->data);
  // assert(api->parameters->get(data->hans_test_audio_1, 0) == 1);
  // assert(api->parameters->get(data->hans_test_audio_2, 0) == 3);
  // assert(api->parameters->get(data->hans_test_audio_2, 1) == 2);
}

void hans_test_audio_new(hans_constructor_api* api, void* buff, size_t size) {
  auto arguments = api->get_args(api);
  for (int i = 0; i < arguments.length; ++i) {
    if (arguments.data[i].type == HANS_NUMBER) {
      api->request_resource(api, HANS_INLET, arguments.data[i].number);
      break;
    }
  }
}

void hans_test_audio_init(void* instance) {
  hans_audio_object* object = static_cast<hans_audio_object*>(instance);
  object->setup = hans_test_audio_setup;
  object->callback = hans_test_audio_callback;
}

extern "C" {
void setup(hans_library_api* api) {
  api->register_object(api, "test-graphics", sizeof(hans_test_gfx_data),
                       hans_test_graphics_new, hans_test_graphics_init,
                       nullptr);

  api->register_object(api, "test-audio", sizeof(hans_test_audio_data),
                       hans_test_audio_new, hans_test_audio_init, nullptr);
}
}
