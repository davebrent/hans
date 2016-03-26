#include "hans/common/Logging.hpp"
#include "hans/engine/ProgramResources.hpp"
#include "hans/engine/ProgramManager.hpp"
#include <catch.hpp>

using namespace hans;

/*
static void gfx_setup(hans_graphics_object* self, hans_object_api* api) {
}

static void gfx_new(hans_graph_object* graph_object, void* buffer, size_t size)
{
  hans_graphics_object* object = static_cast<hans_graphics_object*>(buffer);
  object->setup = gfx_setup;
}

static void aud_setup(hans_audio_object* self, hans_object_api* api) {
}

static void aud_callback(hans_audio_object* self, hans_audio_sample* input,
                         hans_audio_sample* output) {
}

static void aud_new(hans_graph_object* graph_object, void* buffer, size_t size)
{
  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = aud_setup;
  object->callback = aud_callback;
}

SCENARIO("creating and updating programs", "[program]") {
  auto aus = sizeof(hans_audio_object);
  auto gfs = sizeof(hans_graphics_object);

  std::vector<hans_object> audio_objects = {
      {.id = 13, .name = 0, .size = aus, .make = aud_new, .destroy = nullptr},
      {.id = 14, .name = 1, .size = aus, .make = aud_new, .destroy = nullptr},
      {.id = 15, .name = 2, .size = aus, .make = aud_new, .destroy = nullptr}};

  std::vector<hans_object> graphics_objects = {
      {.id = 10, .name = 0, .size = gfs, .make = gfx_new, .destroy = nullptr},
      {.id = 11, .name = 1, .size = gfs, .make = gfx_new, .destroy = nullptr},
      {.id = 12, .name = 2, .size = gfs, .make = gfx_new, .destroy = nullptr}};

  GIVEN("a program manager with no programs") {
    hans_config config = {
        .audio = {.num_channels = 2, .sample_rate = 44100, .block_size = 256},
        .window = {.width = 1184, .height = 640},
        .rpc = {.port = 5555, .num_threads = 1, .requests_per_frame = 10}};

    core::StringManager s(128);
    core::ConsoleLogger logger(HANS_LOG_ERROR);
    std::vector<hans_shader> shaders;
    std::vector<hans_parameter> parameters;

    core::ProgramResources program_resources = {
        .config = &config,
        .logger = &logger,
        .graphics_objects = &graphics_objects,
        .audio_objects = &audio_objects,
        .parameters = &parameters,
        .shaders = &shaders};

    core::ProgramManager program_manager(program_resources);

    WHEN("setting a new program") {
      // 11(1) -> 10(0) -> 12(2)
      hans_graph* gfx = graph::make(3, 2, 0);
      gfx->objects = (hans_graph_object[]){{.object_id = 10, .instance_id = 0},
                                          {.object_id = 11, .instance_id = 1},
                                          {.object_id = 12, .instance_id = 2}};
      gfx->connections = (hans_graph_connection[]){{.source = 1, .sink = 0},
                                                  {.source = 0, .sink = 2}};

      // 14(1) -> 13(0) -> 15(2)
      hans_graph* aud = graph::make(3, 2, 0);
      aud->objects = (hans_graph_object[]){{.object_id = 13, .instance_id = 0},
                                          {.object_id = 14, .instance_id = 1},
                                          {.object_id = 15, .instance_id = 2}};
      aud->connections = (hans_graph_connection[]){{.source = 1, .sink = 0},
                                                  {.source = 0, .sink = 2}};

      THEN("it should succeed") {
        bool res = program_manager.set(s.intern("test-prog"), gfx, aud);
        REQUIRE(res == true);

        graph::destroy(gfx);
        graph::destroy(aud);

        program_manager.process_graphics();
        program_manager.process_audio(nullptr, nullptr);
      }
    }

    WHEN("setting consecutive graphs") {
      THEN("it should succeed") {
        // 11(1) -> 10(0)
        hans_graph* gfx1 = graph::make(2, 1, 0);
        gfx1->objects =
            (hans_graph_object[]){{.object_id = 10, .instance_id = 0},
                                 {.object_id = 11, .instance_id = 1}};
        gfx1->connections = (hans_graph_connection[]){{.source = 1, .sink = 0}};

        // 14(1) -> 13(0)
        hans_graph* aud1 = graph::make(2, 1, 0);
        aud1->objects =
            (hans_graph_object[]){{.object_id = 13, .instance_id = 0},
                                 {.object_id = 14, .instance_id = 1}};
        aud1->connections = (hans_graph_connection[]){{.source = 1, .sink = 0}};

        bool res = program_manager.set(s.intern("test-prog"), gfx1, aud1);
        REQUIRE(res == true);
        program_manager.process_graphics();
        program_manager.process_audio(nullptr, nullptr);

        // --------------------------------------------------------------------

        // 11(1) -> 10(0) -> 12(2)
        hans_graph* gfx2 = graph::make(3, 2, 0);
        gfx2->objects =
            (hans_graph_object[]){{.object_id = 10, .instance_id = 0},
                                 {.object_id = 11, .instance_id = 1},
                                 {.object_id = 12, .instance_id = 2}};
        gfx2->connections = (hans_graph_connection[]){{.source = 1, .sink = 0},
                                                     {.source = 0, .sink = 2}};

        // 14(1) -> 13(0) -> 15(2)
        hans_graph* aud2 = graph::make(3, 2, 0);
        aud2->objects =
            (hans_graph_object[]){{.object_id = 13, .instance_id = 0},
                                 {.object_id = 14, .instance_id = 1},
                                 {.object_id = 15, .instance_id = 2}};
        aud2->connections = (hans_graph_connection[]){{.source = 1, .sink = 0},
                                                     {.source = 0, .sink = 2}};

        res = program_manager.set(s.intern("test-prog"), gfx2, aud2);
        REQUIRE(res == true);
        program_manager.process_graphics();
        program_manager.process_audio(nullptr, nullptr);
      }
    }
  }
}
*/
