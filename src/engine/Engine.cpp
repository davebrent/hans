#include "hans/engine/Engine.hpp"
#include <libguile.h>
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

Engine::Engine(EngineData& ng)
    : settings(ng.settings),
      strings(ng.strings),
      plugins(strings, ng.objects, ng.plugins),
      registers(ng.settings, ng.registers),
      parameters(ng.parameters, ng.parameters_values),
      modulators(parameters, ng.modulators),
      window(),
      shaders(strings, ng.shaders),
      fbos(ng.fbos, ng.fbos_attachments),
      audio_devices(),
      audio_buffers(ng.audio_buffers),
      audio_buses(ng.settings, 1),
      ring_buffers(ng.settings.blocksize, ng.ring_buffers) {
}

struct EngineRunner {
  uint16_t selected_program;

  Engine engine;
  std::vector<std::string>& objects_state;
  std::vector<ObjectDef>& objects;
  std::vector<size_t>& chains;
  std::vector<Program>& programs;

  std::vector<ObjectDef*> graphics_objects;
  std::vector<ObjectDef*> audio_objects;

  AudioStream* audio;
  ReplayRecorder recorder;
  ReplayPlayer player;

  EngineRunner(const EngineRunner& other) = delete;

  EngineRunner(EngineData& ng)
      : engine(ng),
        objects_state(ng.objects_state),
        objects(ng.objects),
        chains(ng.chains),
        programs(ng.programs),
        recorder(ng.parameters_values),
        player(ng, ng.parameters_values) {
    audio = nullptr;
    selected_program = 0;
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

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

static SCM engine_set_program(SCM runner) {
  return SCM_BOOL_F;
}

static SCM engine_frame(EngineRunner& runner, Engine& engine) {
  const auto& program = runner.programs[runner.selected_program];

  engine.modulators.begin();

  for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
    auto id = runner.chains[i];
    for (const auto& object : runner.objects) {
      if (object.id == id) {
        static_cast<GraphicsObject*>(object.instance)->update(engine);
        break;
      }
    }
  }

  runner.recorder.update();

  for (auto i = program.graphics.start; i < program.graphics.end; ++i) {
    auto id = runner.chains[i];
    for (const auto& object : runner.objects) {
      if (object.id == id) {
        static_cast<GraphicsObject*>(object.instance)->draw(engine);
        break;
      }
    }
  }

  engine.modulators.end();
  engine.ring_buffers.advance_all();
  engine.window.update();
  return SCM_BOOL_T;
}

static SCM engine_open__inner(EngineRunner& runner, Engine& engine,
                              Settings& settings) {
  engine.fbos.setup();

  for (auto i = 0; i < runner.objects.size(); ++i) {
    auto& object = runner.objects.at(i);
    auto& state = runner.objects_state.at(i);
    object.instance = object.create(object.id, state);
    static_cast<Object*>(object.instance)->setup(engine);
  }

  auto audio_callback = [&]() {
    const auto& program = runner.programs[runner.selected_program];
    const auto& chain = program.audio;
    for (auto i = chain.start; i < chain.end; ++i) {
      auto id = runner.chains[i];
      for (const auto& object : runner.objects) {
        if (object.id == id) {
          static_cast<AudioObject*>(object.instance)->callback(engine);
          break;
        }
      }
    }
  };

  runner.audio = new AudioStream(settings, engine.audio_devices,
                                 engine.audio_buses, audio_callback);

  if (!runner.audio->open()) {
    std::cerr << "[HANS] Unable to open audio stream" << std::endl;
    return SCM_BOOL_F;
  }

  runner.audio->start();
  return SCM_BOOL_T;
}

static SCM engine_open(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  auto& engine = runner.engine;
  auto& settings = engine.settings;

  if (!engine.window.make("Hans", settings.width, settings.height)) {
    std::cerr << "[HANS] Unable to open window" << std::endl;
    return SCM_BOOL_F;
  }

  return engine_open__inner(runner, engine, settings);
}

static SCM engine_close(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.destroy();
  return SCM_BOOL_T;
}

static SCM engine_run(SCM scm_runner, SCM callback) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  auto& engine = runner.engine;
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
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  engine_frame(runner, runner.engine);
  return SCM_BOOL_T;
}

static SCM engine_capture(SCM scm_runner, SCM frame) {
  auto& engine = scm::to_cpp<EngineRunner>(scm_runner).engine;
  engine.window.capture(scm::to_cpp<Frame>(frame));
  return SCM_BOOL_T;
}

static SCM engine_record_start(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.recorder.start();
  return SCM_BOOL_T;
}

static SCM engine_record_stop(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.recorder.stop();
  runner.player.reset_with_blob(runner.recorder.to_blob());
  return SCM_BOOL_T;
}

static SCM engine_set_engine_frame(SCM scm_runner, SCM frameno) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.player.set(scm_to_int(frameno));
  return SCM_BOOL_T;
}

static SCM engine_set_parameter(SCM runner, SCM id, SCM name, SCM component,
                                SCM value) {
  auto& engine = scm::to_cpp<EngineRunner>(runner).engine;
  if (engine.parameters.set(scm_to_int(id), scm_to_size_t(name),
                            scm_to_int(component), scm_to_double(value))) {
    return SCM_BOOL_T;
  }

  return SCM_BOOL_F;
}

extern "C" {
void scm_init_engine_module() {
  scm::smob<EngineRunner>("engine", [](void* bytes, SCM args) {
    return new (bytes) EngineRunner(scm::to_cpp<EngineData>(scm_car(args)));
  });

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
