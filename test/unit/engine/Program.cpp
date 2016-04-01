#include "hans/engine/Program.hpp"
#include <catch.hpp>
#include "hans/common/Logging.hpp"
#include "hans/common/types.hpp"
#include "hans/graphics/Window.hpp"

using namespace hans;

static void make_gfx_1(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 0);
  api->request_resource(api, HANS_OUTLET, 1);
  api->request_resource(api, HANS_SHADER, 2);

  hans_graphics_object* object = static_cast<hans_graphics_object*>(buffer);
  object->setup = nullptr;
  object->update = nullptr;
  object->draw = nullptr;
}

static void make_gfx_2(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_OUTLET, 0);
  api->request_resource(api, HANS_SHADER, 2);

  hans_graphics_object* object = static_cast<hans_graphics_object*>(buffer);
  object->setup = nullptr;
  object->update = nullptr;
  object->draw = nullptr;
}

static void make_audio_1(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 0);
  api->request_resource(api, HANS_OUTLET, 1);
  api->request_resource(api, HANS_AUDIO_BUFFER, 3);

  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = nullptr;
  object->callback = nullptr;
}

static void make_audio_2(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_OUTLET, 0);
  api->request_resource(api, HANS_AUDIO_BUFFER, 3);

  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = nullptr;
  object->callback = nullptr;
}

static memory::StringManager s(256);

// id, type, name, size, make, destroy
static std::vector<hans_object> GRAPHICS_OBJECTS = {
    {0, HANS_GRAPHICS, s.intern("gfx_object_1"), sizeof(hans_graphics_object),
     make_gfx_1, nullptr},
    {1, HANS_GRAPHICS, s.intern("gfx_object_2"), sizeof(hans_graphics_object),
     make_gfx_2, nullptr}};

static std::vector<hans_object> AUDIO_OBJECTS = {
    {2, HANS_AUDIO, s.intern("audio_object_1"), sizeof(hans_audio_object),
     make_audio_1, nullptr},
    {3, HANS_AUDIO, s.intern("audio_object_2"), sizeof(hans_audio_object),
     make_audio_2, nullptr}};

static std::vector<hans_parameter> PARAMETERS = {
    {0, 0, s.intern("gfx_param_1"), 2, nullptr},
    {1, 0, s.intern("gfx_param_2"), 2, nullptr},
    {2, 0, s.intern("audio_param_1"), 2, nullptr},
    {3, 0, s.intern("audio_param_2"), 2, nullptr}};

static std::vector<hans_shader> SHADERS = {
    {HANS_SHADER_VERTEX, s.intern("vertex-shader-1"),
     s.intern("some vertex shader code")},
    {HANS_SHADER_VERTEX, s.intern("vertex-shader-2"),
     s.intern("some vertex shader code")},
    {HANS_SHADER_FRAGMENT, s.intern("fragment-shader-1"),
     s.intern("some fragment shader code")}};

static std::vector<hans_fbo> FRAME_BUFFERS = {};

TEST_CASE("creating and setting programs", "[program]") {
  common::ConsoleLogger logger(common::Logger::ERROR);
  audio::AudioBufferManager audio_buffer_manager(256);
  audio::AudioBusManager audio_bus_manager(audio_buffer_manager);

  engine::ProgramResources program_resources;
  program_resources.config = nullptr;
  program_resources.logger = &logger;
  program_resources.strings = &s;
  program_resources.graphics_objects = &GRAPHICS_OBJECTS;
  program_resources.audio_objects = &AUDIO_OBJECTS;
  program_resources.parameters = &PARAMETERS;
  program_resources.shaders = &SHADERS;
  program_resources.frame_buffers = &FRAME_BUFFERS;
  program_resources.audio_buffers = &audio_buffer_manager;
  program_resources.audio_buses = &audio_bus_manager;

  engine::Program program(program_resources);
  graphics::Window window("", 1, 1);

  SECTION("multiple instances of the same object") {
    common::ObjectGraph audio_graph(2, 0, 0);
    audio_graph.object_at(0)->object_id = 2;
    audio_graph.object_at(1)->object_id = 2;

    common::ObjectGraph graphics_graph(2, 0, 0);
    graphics_graph.object_at(0)->object_id = 1;
    graphics_graph.object_at(1)->object_id = 1;

    REQUIRE(program.set(audio_graph, graphics_graph) == true);

    REQUIRE(audio_graph.object_at(0)->instance_id == 1);
    REQUIRE(audio_graph.object_at(1)->instance_id == 0);

    REQUIRE(graphics_graph.object_at(0)->instance_id == 3);
    REQUIRE(graphics_graph.object_at(1)->instance_id == 2);
  }

  SECTION("repeated calls with multiple instances of the same object") {
    // Call 1
    common::ObjectGraph audio_graph_1(1, 0, 0);
    audio_graph_1.object_at(0)->object_id = 2;

    common::ObjectGraph graphics_graph_1(1, 0, 0);
    graphics_graph_1.object_at(0)->object_id = 1;

    REQUIRE(program.set(audio_graph_1, graphics_graph_1) == true);
    REQUIRE(audio_graph_1.object_at(0)->instance_id == 0);
    REQUIRE(graphics_graph_1.object_at(0)->instance_id == 1);

    // Call 2
    common::ObjectGraph audio_graph_2(2, 0, 0);
    audio_graph_2.object_at(0)->object_id = 2;
    audio_graph_2.object_at(1)->object_id = 2;

    common::ObjectGraph graphics_graph_2(2, 0, 0);
    graphics_graph_2.object_at(0)->object_id = 1;
    graphics_graph_2.object_at(1)->object_id = 1;

    REQUIRE(program.set(audio_graph_2, graphics_graph_2) == true);
    REQUIRE(audio_graph_2.object_at(0)->instance_id == 1);
    REQUIRE(audio_graph_2.object_at(1)->instance_id == 0);
    REQUIRE(graphics_graph_2.object_at(0)->instance_id == 3);
    REQUIRE(graphics_graph_2.object_at(1)->instance_id == 2);

    // Call 3
    common::ObjectGraph audio_graph_3(3, 0, 0);
    audio_graph_3.object_at(0)->object_id = 2;
    audio_graph_3.object_at(1)->object_id = 2;
    audio_graph_3.object_at(2)->object_id = 2;

    common::ObjectGraph graphics_graph_3(3, 0, 0);
    graphics_graph_3.object_at(0)->object_id = 1;
    graphics_graph_3.object_at(1)->object_id = 1;
    graphics_graph_3.object_at(2)->object_id = 1;

    REQUIRE(program.set(audio_graph_3, graphics_graph_3) == true);
    REQUIRE(audio_graph_3.object_at(0)->instance_id == 2);
    REQUIRE(audio_graph_3.object_at(1)->instance_id == 1);
    REQUIRE(audio_graph_3.object_at(2)->instance_id == 0);
    REQUIRE(graphics_graph_3.object_at(0)->instance_id == 5);
    REQUIRE(graphics_graph_3.object_at(1)->instance_id == 4);
    REQUIRE(graphics_graph_3.object_at(2)->instance_id == 3);
  }

  SECTION("setting a programs graphs") {
    common::ObjectGraph graphics_graph(2, 1, 0);
    graphics_graph.object_at(1)->object_id = 0;
    graphics_graph.object_at(0)->object_id = 1;

    graphics_graph.connection_at(0)->source = 1;
    graphics_graph.connection_at(0)->outlet = 0;
    graphics_graph.connection_at(0)->sink = 0;
    graphics_graph.connection_at(0)->inlet = 0;

    common::ObjectGraph audio_graph(2, 1, 0);
    audio_graph.object_at(0)->object_id = 2;
    audio_graph.object_at(1)->object_id = 3;

    audio_graph.connection_at(0)->source = 0;
    audio_graph.connection_at(0)->outlet = 0;
    audio_graph.connection_at(0)->sink = 1;
    audio_graph.connection_at(0)->inlet = 0;

    REQUIRE(program.set(audio_graph, graphics_graph) == true);

    REQUIRE(audio_graph.object_at(0)->instance_id == 0);
    REQUIRE(audio_graph.object_at(0)->inlets == 0);
    REQUIRE(audio_graph.object_at(0)->outlets == 1);

    REQUIRE(audio_graph.object_at(1)->instance_id == 1);
    REQUIRE(audio_graph.object_at(1)->inlets == 1);
    REQUIRE(audio_graph.object_at(1)->outlets == 0);

    REQUIRE(graphics_graph.object_at(0)->instance_id == 3);
    REQUIRE(graphics_graph.object_at(0)->inlets == 0);
    REQUIRE(graphics_graph.object_at(0)->outlets == 1);

    REQUIRE(graphics_graph.object_at(1)->instance_id == 2);
    REQUIRE(graphics_graph.object_at(1)->inlets == 1);
    REQUIRE(graphics_graph.object_at(1)->outlets == 0);
  }
}
