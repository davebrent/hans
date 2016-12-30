#include "hans/common/scm.hpp"
#include <libguile.h>
#include <cassert>
#include <stdexcept>
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"
#include "hans/common/types.hpp"

using namespace hans;

SCM common::hans_hash(SCM str, SCM hex) {
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

static bool str_eqaul_sym(const char* str, SCM sym) {
  assert(scm_is_true(scm_symbol_p(sym)) == 1);
  return scm_is_true(scm_eq_p(scm_from_locale_symbol(str), sym)) == 1;
}

static Frame::Format scm_to_fmt(SCM value) {
  if (str_eqaul_sym("rgba", value)) {
    return Frame::RGBA;
  } else if (str_eqaul_sym("rgb", value)) {
    return Frame::RGB;
  }
  throw std::runtime_error("Unknown frame format");
}

extern "C" {
void scm_init_common_module() {
  scm::procedure<common::hans_hash>("hans-hash", 1, 1, 0);

  scm::smob<common::Config>("config");
  scm::smob<Plugin>("plugin");
  scm::smob<ObjectDef>("object");
  scm::smob<Chain>("chain");
  scm::smob<Program>("program");
  scm::smob<Register>("register");
  scm::smob<Argument>("argument");
  scm::smob<Parameter>("parameter");
  scm::smob<Modulator>("modulator");
  scm::smob<RingBuffer>("ring-buffer");
  scm::smob<audio::Buffer>("audio-buffer");
  scm::smob<graphics::Shader>("shader");
  scm::smob<graphics::FBO::Attachment>("fbo-attachment");
  scm::smob<graphics::FBO>("fbo");

  scm::smob<Frame>("frame", [](void* bytes, SCM args) {
    auto width = scm_to_int(scm_list_ref(args, scm_from_int(0)));
    auto height = scm_to_int(scm_list_ref(args, scm_from_int(1)));
    auto fmt = scm_to_fmt(scm_list_ref(args, scm_from_int(2)));
    return new (bytes) Frame(width, height, fmt);
  });
}
}
