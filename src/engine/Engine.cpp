#include "hans/engine/Engine.hpp"
#include <GLFW/glfw3.h>
#include <libguile.h>
#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/AudioDevices.hpp"
#include "hans/audio/AudioStream.hpp"
#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/DataLoader.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/hasher.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/engine/object.hpp"
#include "hans/graphics/Window.hpp"
#include "hans/graphics/gl.h"

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

static void error_callback(int error, const char* description) {
  std::cout << "Window Error: " << error << " " << description << std::endl;
}

Engine::Engine(Config& _config, DataReader* reader)
    : config(_config),
      strings(reader->data.string_hashes, reader->data.string_offsets,
              reader->data.strings),
      libraries(strings, reader->data.objects, reader->data.libraries),
      registers(config, reader->data.registers),
      parameters(reader->data.parameters, reader->data.parameter_values),
      modulators(parameters, reader->data.modulators),
      window(),
      shaders(strings, reader->data.shaders),
      fbos(reader->data.fbos, reader->data.fbo_attachments),
      audio_devices(),
      audio_buffers(reader->data.audio_buffers),
      audio_buses(config, 1),
      ring_buffers(config, reader->data.ring_buffers) {
}

class EngineRunner {
 public:
  uint16_t selected_program;

  Engine engine;
  common::DataFile::Blob object_data;
  common::ListView<ObjectDef> objects;
  common::ListView<size_t> chains;
  common::ListView<Program> programs;

  EngineRunner(Config& config, DataReader* reader)
      : m_reader(reader),
        engine(config, reader),
        object_data(reader->data.object_data),
        objects(reader->data.objects),
        chains(reader->data.chains),
        programs(reader->data.programs) {
    selected_program = 0;
  }

  ~EngineRunner() {
    delete m_reader;
  }

  // Objects must be destroyed before library destructor is called
  void destroy() {
    for (const auto& object : objects) {
      object.destroy(object.instance);
    }
  }

 private:
  common::DataReader* m_reader;
};

static scm_t_bits EngineTag;

static EngineRunner* scm_to_engine(SCM engine) {
  scm_assert_smob_type(EngineTag, engine);
  return reinterpret_cast<EngineRunner*>(SCM_SMOB_DATA(engine));
}

static SCM make_engine(SCM filepath) {
  auto place = scm_gc_malloc(sizeof(EngineRunner), "engine");

  Config config;
  config.channels = 2;
  config.samplerate = 44100;
  config.blocksize = 64;
  config.width = 640;
  config.height = 360;

  auto path = scm_to_locale_string(filepath);
  auto runner = new (place) EngineRunner(config, new DataReader(path));
  free(path);
  return scm_new_smob(EngineTag, (scm_t_bits)runner);
}

static SCM engine_set_program(SCM runner) {
  return SCM_BOOL_F;
}

static SCM engine_run(SCM runner) {
  auto instance = scm_to_engine(runner);
  Engine& engine = instance->engine;
  Config& config = engine.config;

  if (!engine.window.make("Hans", config.width, config.height)) {
    std::cerr << "Unable to open window" << std::endl;
    return SCM_BOOL_F;
  }

  engine.fbos.setup();

  auto state = instance->object_data.data;
  for (auto& object : instance->objects) {
    object.instance = object.create(object.id, state);
    static_cast<Object*>(object.instance)->setup(engine);
    state = static_cast<void*>(static_cast<char*>(state) + object.size);
  }

  auto audio_stream =
      AudioStream(config, engine.audio_devices, engine.audio_buses, [&]() {
        const auto& program = instance->programs[instance->selected_program];
        const auto& chain = program.audio;

        for (auto i = chain.start; i < chain.end; ++i) {
          auto id = instance->chains[i];
          for (const auto& object : instance->objects) {
            if (object.id == id) {
              auto instance = static_cast<AudioObject*>(object.instance);
              instance->callback(engine);
              break;
            }
          }
        }
      });

  if (!audio_stream.open()) {
    std::cerr << "Unable to open audio stream" << std::endl;
    return SCM_BOOL_F;
  }

  audio_stream.start();

  while (!engine.window.should_close()) {
    const auto& program = instance->programs[instance->selected_program];

    engine.modulators.begin();

    for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
      auto id = instance->chains[i];
      for (const auto& object : instance->objects) {
        if (object.id == id) {
          auto instance = static_cast<GraphicsObject*>(object.instance);
          instance->update(engine);
          break;
        }
      }
    }

    for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
      auto id = instance->chains[i];
      for (const auto& object : instance->objects) {
        if (object.id == id) {
          auto instance = static_cast<GraphicsObject*>(object.instance);
          instance->draw(engine);
          break;
        }
      }
    }

    engine.modulators.end();
    engine.ring_buffers.advance_all();
    engine.window.update();
  }

  audio_stream.close();
  instance->destroy();
  return SCM_BOOL_T;
}

static SCM engine_destroy(SCM runner) {
  return SCM_BOOL_F;
}

extern "C" {
void scm_init_engine_module() {
  EngineTag = scm_make_smob_type("engine", sizeof(EngineRunner));
  scm_c_define_gsubr("make-engine", 1, 0, 0, (scm_t_subr)make_engine);
  scm_c_define_gsubr("set-engine-program!", 2, 0, 0,
                     (scm_t_subr)engine_set_program);
  scm_c_define_gsubr("engine-run", 1, 0, 0, (scm_t_subr)engine_run);
  scm_c_define_gsubr("engine-destroy", 1, 0, 0, (scm_t_subr)engine_destroy);
}
}
