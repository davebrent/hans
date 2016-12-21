#include "hans/engine/Engine.hpp"
#include <libguile.h>
#include "hans/common/DataLoader.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"
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
#include "hans/engine/replay.hpp"

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
  ReplayRecorder recorder;
  ReplayPlayer player;

  EngineRunner(Config& config, DataReader* reader)
      : m_reader(reader),
        engine(config, reader),
        object_data(reader->data.object_data),
        objects(reader->data.objects),
        chains(reader->data.chains),
        programs(reader->data.programs),
        recorder(reader->data.parameter_values),
        player(reader->data.parameter_values) {
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

    engine.fbos.destroy();
    engine.shaders.destroy();

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
  std::free(path);
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

  runner->recorder.update();

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

static SCM engine_close(SCM scm_runner) {
  auto runner = scm_to_engine_runner(scm_runner);
  runner->destroy();
  runner->~EngineRunner();
  scm_gc_free(runner, sizeof(EngineRunner), "engine");
  return SCM_BOOL_T;
}

static SCM engine_run(SCM scm_runner, SCM callback) {
  auto runner = scm_to_engine_runner(scm_runner);
  auto& engine = runner->engine;
  auto with_callback = scm_is_true(scm_procedure_p(callback)) == 1;

  while (!engine.window.should_close()) {
    if (with_callback) {
      if (scm_is_false(scm_call_0(callback)) == 1) {
        break;
      }
    }

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
  engine.window.capture(scm::to_cpp<Frame>(frame));
  return SCM_BOOL_T;
}

static SCM engine_record_start(SCM scm_runner) {
  auto runner = scm_to_engine_runner(scm_runner);
  runner->recorder.start();
  return SCM_BOOL_T;
}

static SCM engine_record_stop(SCM scm_runner) {
  auto runner = scm_to_engine_runner(scm_runner);
  runner->recorder.stop();
  runner->player.reset_with_blob(runner->recorder.to_blob());
  return SCM_BOOL_T;
}

static SCM engine_set_engine_frame(SCM scm_runner, SCM frameno) {
  auto runner = scm_to_engine_runner(scm_runner);
  runner->player.set(scm_to_int(frameno));
  return SCM_BOOL_T;
}

static SCM engine_set_parameter(SCM runner, SCM id, SCM name, SCM component,
                                SCM value) {
  auto& engine = scm_to_engine_runner(runner)->engine;

  if (engine.parameters.set(scm_to_int(id), scm_to_size_t(name),
                            scm_to_int(component), scm_to_double(value))) {
    return SCM_BOOL_T;
  }

  return SCM_BOOL_F;
}

extern "C" {
void scm_init_engine_module() {
  EngineTag = scm_make_smob_type("engine", sizeof(EngineRunner));
  scm::procedure<make_engine>("make-engine", 1, 0, 0);
  scm::procedure<engine_set_program>("set-engine-program!", 2, 0, 0);
  scm::procedure<engine_open>("engine-open", 1, 0, 0);
  scm::procedure<engine_close>("engine-close", 1, 0, 0);
  scm::procedure<engine_tick>("engine-tick", 1, 0, 0);
  scm::procedure<engine_run>("engine-run", 1, 1, 0);
  scm::procedure<engine_capture>("engine-capture", 2, 0, 0);
  scm::procedure<engine_record_start>("engine-record-start", 1, 0, 0);
  scm::procedure<engine_record_stop>("engine-record-stop", 1, 0, 0);
  scm::procedure<engine_set_engine_frame>("set-engine-frame!", 2, 0, 0);
  scm::procedure<engine_set_parameter>("set-engine-parameter!", 5, 0, 0);
}
}
