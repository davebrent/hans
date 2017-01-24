#include "hans/engine/Engine.hpp"
#include <libguile.h>
#include <algorithm>
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
      plugins(strings, ng.plugins),
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

template <typename T>
static void construct(std::vector<T>& output, Graphs& input, Engine& engine) {
  std::vector<T> table;
  table.reserve(input.objects.size());
  output.reserve(input.indices.size());

  for (auto i = 0; i < input.objects.size(); ++i) {
    auto& object = input.objects.at(i);
    auto& state = input.states.at(i);
    auto instance = engine.plugins.construct(object.name, object.id, state);
    instance->setup(engine);
    table.push_back(static_cast<T>(instance));
  }

  for (const auto index : input.indices) {
    output.push_back(table.at(index));
  }
}

template <typename T>
static void destruct(std::vector<T>& instances, Graphs& graph, Engine& engine) {
  std::vector<T> table;
  table.reserve(graph.objects.size());

  for (const auto instance : instances) {
    if (std::find(table.begin(), table.end(), instance) == table.end()) {
      table.push_back(instance);
    }
  }

  for (auto i = 0; i < graph.objects.size(); ++i) {
    auto& object = graph.objects.at(i);
    engine.plugins.destruct(object.name, table.at(i));
  }

  instances.clear();
}

struct EngineRunner {
  uint16_t program;

  Engine engine;

  Programs& programs;
  std::vector<GraphicsObject*> graphics_objects;
  std::vector<AudioObject*> audio_objects;

  AudioStream* stream;
  ReplayRecorder recorder;
  ReplayPlayer player;

  EngineRunner(const EngineRunner& other) = delete;

  EngineRunner(EngineData& ng)
      : engine(ng),
        programs(ng.programs),
        recorder(ng.parameters_values, ng.recordings),
        player(ng.parameters_values, ng.recordings) {
    stream = nullptr;
    program = 0;
  }

  void destroy() {
    if (stream != nullptr) {
      stream->close();
      delete stream;
      stream = nullptr;
    }

    destruct<AudioObject*>(audio_objects, programs.audio, engine);
    destruct<GraphicsObject*>(graphics_objects, programs.graphics, engine);

    engine.fbos.destroy();
    engine.shaders.destroy();
  }

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

static SCM engine_set_program(SCM runner) {
  return SCM_BOOL_F;
}

static SCM engine_frame(EngineRunner& runner, Engine& engine) {
  engine.modulators.begin();

  auto range = runner.programs.graphics.ranges.at(runner.program);
  for (auto i = range.start; i < range.end; ++i) {
    runner.graphics_objects.at(i)->update(engine);
  }

  runner.player.tick();
  runner.recorder.tick();

  for (auto i = range.start; i < range.end; ++i) {
    runner.graphics_objects.at(i)->draw(engine);
  }

  engine.modulators.end();
  engine.ring_buffers.advance_all();
  engine.window.update();
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

  engine.fbos.setup();

  construct<AudioObject*>(runner.audio_objects, runner.programs.audio, engine);
  construct<GraphicsObject*>(runner.graphics_objects, runner.programs.graphics,
                             engine);

  auto audio_callback = [&]() {
    auto range = runner.programs.audio.ranges.at(runner.program);
    for (auto i = range.start; i < range.end; ++i) {
      runner.audio_objects.at(i)->callback(engine);
    }
  };

  runner.stream = new AudioStream(settings, engine.audio_devices,
                                  engine.audio_buses, audio_callback);

  if (!runner.stream->open()) {
    std::cerr << "[HANS] Unable to open audio stream" << std::endl;
    return SCM_BOOL_F;
  }

  runner.stream->start();
  return SCM_BOOL_T;
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
  return SCM_BOOL_T;
}

static SCM engine_player_start(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.player.start();
  return SCM_BOOL_T;
}

static SCM engine_player_stop(SCM scm_runner) {
  auto& runner = scm::to_cpp<EngineRunner>(scm_runner);
  runner.player.stop();
  return SCM_BOOL_T;
}

static SCM engine_player_set(SCM scm_runner, SCM frameno) {
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
  scm::procedure<engine_player_start>("engine-player-start", 1, 0, 0);
  scm::procedure<engine_player_stop>("engine-player-stop", 1, 0, 0);
  scm::procedure<engine_player_set>("set-engine-player!", 2, 0, 0);
  scm::procedure<engine_set_parameter>("set-engine-parameter!", 5, 0, 0);
}
}
