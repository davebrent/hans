#include "hans/engine/Engine.hpp"
#include <libguile.h>
#include "hans/common/DataLoader.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/scm.hpp"
#include "hans/engine/AudioBufferManager.hpp"
#include "hans/engine/AudioBusManager.hpp"
#include "hans/engine/AudioDevices.hpp"
#include "hans/engine/AudioStream.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/PluginManager.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/engine/RingBufferManager.hpp"
#include "hans/engine/Window.hpp"
#include "hans/engine/object.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

Engine::Engine(Config& _config, DataReader* reader)
    : config(_config),
      strings(reader->data.string_hashes, reader->data.string_offsets,
              reader->data.strings),
      plugins(strings, reader->data.objects, reader->data.plugins),
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

  AudioStream* audio;

  EngineRunner(Config& config, DataReader* reader)
      : m_reader(reader),
        engine(config, reader),
        object_data(reader->data.object_data),
        objects(reader->data.objects),
        chains(reader->data.chains),
        programs(reader->data.programs) {
    audio = nullptr;
    selected_program = 0;
  }

  ~EngineRunner() {
    delete m_reader;
  }

  void destroy() {
    if (audio != nullptr) {
      audio->close();
      delete audio;
      audio = nullptr;
    }

    for (const auto& object : objects) {
      object.destroy(object.instance);
    }
  }

 private:
  common::DataReader* m_reader;
};

static scm_t_bits EngineTag;

static EngineRunner* scm_to_engine_runner(SCM engine) {
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

// One iteration of the realtime loop
static SCM engine_frame(EngineRunner* runner, Engine& engine) {
  const auto& program = runner->programs[runner->selected_program];

  engine.modulators.begin();

  for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
    auto id = runner->chains[i];
    for (const auto& object : runner->objects) {
      if (object.id == id) {
        auto instance = static_cast<GraphicsObject*>(object.instance);
        instance->update(engine);
        break;
      }
    }
  }

  for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
    auto id = runner->chains[i];
    for (const auto& object : runner->objects) {
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
  return SCM_BOOL_T;
}

static SCM engine_open__inner(EngineRunner* runner, Engine& engine,
                              Config& config) {
  engine.fbos.setup();

  auto state = runner->object_data.data;
  for (auto& object : runner->objects) {
    object.instance = object.create(object.id, state);
    static_cast<Object*>(object.instance)->setup(engine);
    state = static_cast<void*>(static_cast<char*>(state) + object.size);
  }

  auto audio_callback = [&]() {
    const auto& program = runner->programs[runner->selected_program];
    const auto& chain = program.audio;

    for (auto i = chain.start; i < chain.end; ++i) {
      auto id = runner->chains[i];
      for (const auto& object : runner->objects) {
        if (object.id == id) {
          auto instance = static_cast<AudioObject*>(object.instance);
          instance->callback(engine);
          break;
        }
      }
    }
  };

  runner->audio = new AudioStream(config, engine.audio_devices,
                                  engine.audio_buses, audio_callback);

  if (!runner->audio->open()) {
    std::cerr << "Unable to open audio stream" << std::endl;
    return SCM_BOOL_F;
  }

  runner->audio->start();
  return SCM_BOOL_T;
}

static SCM engine_open(SCM runner) {
  auto instance = scm_to_engine_runner(runner);
  auto& engine = instance->engine;
  auto& config = engine.config;

  if (!engine.window.make("Hans", config.width, config.height)) {
    std::cerr << "Unable to open window" << std::endl;
    return SCM_BOOL_F;
  }

  // XXX: Ensures other GL classes are destroyed before the window & context
  return engine_open__inner(instance, engine, config);
}

static SCM engine_close(SCM runner) {
  scm_to_engine_runner(runner)->destroy();
  return SCM_BOOL_T;
}

static SCM engine_run(SCM scm_runner) {
  auto runner = scm_to_engine_runner(scm_runner);
  auto& engine = runner->engine;

  while (!engine.window.should_close()) {
    engine_frame(runner, engine);
  }

  return SCM_BOOL_T;
}

static SCM engine_tick(SCM scm_runner) {
  auto runner = scm_to_engine_runner(scm_runner);
  auto& engine = runner->engine;
  engine_frame(runner, engine);
  return SCM_BOOL_T;
}

static SCM engine_capture(SCM runner, SCM frame) {
  auto& engine = scm_to_engine_runner(runner)->engine;
  engine.window.capture(scm_to_frame(frame));
  return SCM_BOOL_T;
}

extern "C" {
void scm_init_engine_module() {
  EngineTag = scm_make_smob_type("engine", sizeof(EngineRunner));
  scm_c_define_gsubr("make-engine", 1, 0, 0, (scm_t_subr)make_engine);

  scm_c_define_gsubr("set-engine-program!", 2, 0, 0,
                     (scm_t_subr)engine_set_program);

  scm_c_define_gsubr("engine-open", 1, 0, 0, (scm_t_subr)engine_open);
  scm_c_define_gsubr("engine-close", 1, 0, 0, (scm_t_subr)engine_close);

  scm_c_define_gsubr("engine-tick", 1, 0, 0, (scm_t_subr)engine_tick);
  scm_c_define_gsubr("engine-run", 1, 0, 0, (scm_t_subr)engine_run);

  scm_c_define_gsubr("engine-capture", 2, 0, 0, (scm_t_subr)engine_capture);
}
}
