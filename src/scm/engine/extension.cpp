#include <libguile.h>
#include <new>
#include "hans/engine/configurator.hpp"
#include "hans/engine/engine.hpp"
#include "hans/engine/hasher.hpp"
#include "hans/engine/image.hpp"
#include "hans/engine/video.hpp"
#include "hans/scm/procedure.hpp"
#include "hans/scm/smobs.hpp"

using namespace hans;
using namespace hans::engine;

static SCM engine_set_program(SCM engine, SCM program) {
  auto& ng = scm::to_cpp<Engine>(engine);
  ng.set_program(scm_to_size_t(program));
  return SCM_BOOL_T;
}

static SCM engine_set_parameter(SCM engine, SCM id, SCM name, SCM component,
                                SCM value) {
  auto& ng = scm::to_cpp<Engine>(engine);
  if (ng.set_parameter(scm_to_size_t(id), scm_to_size_t(name),
                       scm_to_size_t(component), scm_to_double(value))) {
    return SCM_BOOL_T;
  }

  return SCM_BOOL_F;
}

static SCM engine_setup(SCM engine) {
  if (scm::to_cpp<Engine>(engine).setup()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_destroy(SCM engine) {
  if (scm::to_cpp<Engine>(engine).destroy()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_tick_audio(SCM engine) {
  scm::to_cpp<Engine>(engine).tick_audio();
  return SCM_BOOL_T;
}

static SCM engine_tick_graphics(SCM engine) {
  scm::to_cpp<Engine>(engine).tick_graphics();
  return SCM_BOOL_T;
}

static SCM engine_capture(SCM engine, SCM frame) {
  if (scm::to_cpp<Engine>(engine).capture(scm::to_cpp<Frame>(frame))) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_record_start(SCM engine) {
  if (scm::to_cpp<Engine>(engine).record_start()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_record_stop(SCM engine) {
  if (scm::to_cpp<Engine>(engine).record_stop()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_player_start(SCM engine) {
  if (scm::to_cpp<Engine>(engine).player_start()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_player_set(SCM engine, SCM frameno) {
  if (scm::to_cpp<Engine>(engine).player_set(scm_to_size_t(frameno))) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_player_stop(SCM engine) {
  if (scm::to_cpp<Engine>(engine).player_stop()) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM engine_run_forever(SCM engine, SCM callback) {
  auto& ng = scm::to_cpp<Engine>(engine);

  if (scm_is_true(scm_procedure_p(callback)) == 1) {
    callback = scm_gc_protect_object(callback);

    ng.run_forever([callback]() -> bool {
      return scm_is_true(scm_call_0(callback)) == 1;
    });

    callback = scm_gc_unprotect_object(callback);
  } else {
    ng.run_forever();
  }

  return SCM_BOOL_T;
}

static SCM image_encode(SCM frame, SCM filepath) {
  auto path = scm_to_locale_string(filepath);
  auto result = image::encode(path, scm::to_cpp<Frame>(frame));
  std::free(path);
  return (result) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM video_encoder_encode(SCM scm_encoder, SCM scm_frame) {
  auto& encoder = scm::to_cpp<VideoEncoder>(scm_encoder);
  auto& frame = scm::to_cpp<Frame>(scm_frame);
  if (encoder.encode(frame)) {
    return SCM_BOOL_T;
  }
  return SCM_BOOL_F;
}

static SCM video_encoder_close(SCM scm_encoder) {
  scm::to_cpp<VideoEncoder>(scm_encoder).close();
  return SCM_BOOL_T;
}

static SCM hans_hash(SCM str, SCM hex) {
  if (scm_is_true(scm_symbol_p(str)) == 1) {
    str = scm_symbol_to_string(str);
  }

  auto data = scm_to_locale_string(str);
  auto hash = engine::hasher(data);
  free(data);

  auto value = scm_from_uint64(hash);
  if (scm_is_true(scm_boolean_p(hex)) == 1 && scm_is_true(hex) == 1) {
    auto prefix = scm_from_locale_string("0x");
    auto suffix = scm_number_to_string(value, scm_from_int(16));
    return scm_string_append(scm_list_2(prefix, suffix));
  }

  return value;
}

extern "C" {
void scm_init_hans_engine() {
  // clang-format off
  scm::define_enum("OBJECTS", {
    {"audio", ObjectDef::AUDIO},
    {"graphics", ObjectDef::GRAPHICS}
  });

  scm::define_enum("RESOURCES", {
    {"parameter", engine::Configurator::Resources::PARAMETER},
    {"shader", engine::Configurator::Resources::SHADER},
    {"audio-buffer", engine::Configurator::Resources::AUDIO_BUFFER},
    {"inlet", engine::Configurator::Resources::INLET},
    {"outlet", engine::Configurator::Resources::OUTLET},
    {"ring-buffer", engine::Configurator::Resources::RING_BUFFER}
  });

  scm::define_enum("REGISTERS", {
    {"inlet", Register::INLET},
    {"outlet", Register::OUTLET}
  });

  scm::define_enum("ARGUMENTS", {
    {"boolean", Argument::BOOLEAN},
    {"number", Argument::NUMBER},
    {"string", Argument::STRING}
  });

  scm::define_enum("SHADERS", {
    {"vertex", graphics::Shader::VERTEX},
    {"fragment", graphics::Shader::FRAGMENT}
  });

  scm::define_enum("FBO_ATTACHMENTS", {
    {"color", graphics::FBO::Attachment::COLOR},
    {"depth", graphics::FBO::Attachment::DEPTH},
    {"stencil", graphics::FBO::Attachment::STENCIL}
  });

  // clang-format on

  scm::procedure<hans_hash>("hans-hash", 1, 1, 0);

  scm::smob<VideoEncoder>("video-encoder", [](void* bytes, SCM args) {
    auto path = scm_to_locale_string(scm_list_ref(args, scm_from_int(0)));
    auto width = scm_to_int(scm_list_ref(args, scm_from_int(1)));
    auto height = scm_to_int(scm_list_ref(args, scm_from_int(2)));
    auto result = new (bytes) VideoEncoder(path, width, height);
    std::free(path);
    return result;
  });

  scm::procedure<video_encoder_encode>("%video-encoder-encode", 2, 0, 0);
  scm::procedure<video_encoder_close>("%video-encoder-close", 1, 0, 0);
  scm::procedure<image_encode>("image-encode", 2, 0, 0);

  scm::smob<Graphs>("graphs");
  scm::smob<Plugins>("plugins");
  scm::smob<Arguments>("arguments");
  scm::smob<Strings>("strings");
  scm::smob<Settings>("settings");
  scm::smob<Recordings>("recordings");
  scm::smob<EngineData>("engine-data");

  scm::smob<Frame>("frame", [](void* bytes, SCM args) {
    auto width = scm_to_int(scm_list_ref(args, scm_from_int(0)));
    auto height = scm_to_int(scm_list_ref(args, scm_from_int(1)));
    return new (bytes) Frame(width, height);
  });

  scm::smob<Engine>("engine", [](void* bytes, SCM args) {
    auto data = scm::to_cpp<EngineData>(scm_list_ref(args, scm_from_int(0)));
    return new (bytes) Engine(data);
  });
  scm::procedure<engine_set_parameter>("set-engine-parameter!", 5, 0, 0);
  scm::procedure<engine_set_program>("set-engine-program!", 2, 0, 0);
  scm::procedure<engine_setup>("engine-setup", 1, 0, 0);
  scm::procedure<engine_destroy>("engine-destroy", 1, 0, 0);
  scm::procedure<engine_tick_audio>("engine-tick-audio", 1, 0, 0);
  scm::procedure<engine_tick_graphics>("engine-tick-graphics", 1, 0, 0);
  scm::procedure<engine_run_forever>("engine-run-forever", 1, 1, 0);
  scm::procedure<engine_capture>("engine-capture", 1, 0, 0);
  scm::procedure<engine_record_start>("engine-record-start", 1, 0, 0);
  scm::procedure<engine_record_stop>("engine-record-stop", 1, 0, 0);
  scm::procedure<engine_player_start>("engine-player-start", 1, 0, 0);
  scm::procedure<engine_player_set>("set-engine-player!", 2, 0, 0);
  scm::procedure<engine_player_stop>("engine-player-stop", 1, 0, 0);
}
}
