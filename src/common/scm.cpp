#include "hans/common/scm.hpp"
#include <libguile.h>
#include <cassert>
#include <stdexcept>
#include "hans/common/primitives.hpp"
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"
#include "hans/engine/Patcher.hpp"

using namespace hans;

SCM common::hans_hash(SCM str, SCM hex) {
  if (scm_is_true(scm_symbol_p(str)) == 1) {
    str = scm_symbol_to_string(str);
  }

  auto data = scm_to_locale_string(str);
  auto hash = common::hasher(data);
  free(data);

  auto value = scm_from_uint64(hash);
  if (scm_is_true(scm_boolean_p(hex)) == 1 && scm_is_true(hex) == 1) {
    auto prefix = scm_from_locale_string("0x");
    auto suffix = scm_number_to_string(value, scm_from_int(16));
    return scm_string_append(scm_list_2(prefix, suffix));
  }

  return value;
}

void scm_init_common_module() {
  scm::procedure<common::hans_hash>("hans-hash", 1, 1, 0);

  scm::smob<Settings>("settings");
  scm::smob<Strings>("strings");
  scm::smob<Plugin>("plugin");
  scm::smob<ObjectDef>("object");
  scm::smob<Chain>("chain");
  scm::smob<Program>("program");
  scm::smob<Register>("register");
  scm::smob<Arguments>("arguments");
  scm::smob<Parameter>("parameter");
  scm::smob<Modulator>("modulator");
  scm::smob<RingBuffer>("ring-buffer");
  scm::smob<audio::Buffer>("audio-buffer");
  scm::smob<graphics::Shader>("shader");
  scm::smob<graphics::FBO::Attachment>("fbo-attachment");
  scm::smob<graphics::FBO>("fbo");
  scm::smob<EngineData>("engine-data");

  scm::smob<Frame>("frame", [](void* bytes, SCM args) {
    auto width = scm_to_int(scm_list_ref(args, scm_from_int(0)));
    auto height = scm_to_int(scm_list_ref(args, scm_from_int(1)));
    return new (bytes) Frame(width, height);
  });

  // clang-format off
  scm::define_enum("OBJECTS", {
    {"audio", ObjectDef::AUDIO},
    {"graphics", ObjectDef::GRAPHICS}
  });

  scm::define_enum("RESOURCES", {
    {"parameter", engine::IPatcher::Resources::PARAMETER},
    {"shader", engine::IPatcher::Resources::SHADER},
    {"audio-buffer", engine::IPatcher::Resources::AUDIO_BUFFER},
    {"inlet", engine::IPatcher::Resources::INLET},
    {"outlet", engine::IPatcher::Resources::OUTLET},
    {"ring-buffer", engine::IPatcher::Resources::RING_BUFFER}
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
}
