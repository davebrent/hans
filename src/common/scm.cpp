#include "hans/common/scm.hpp"
#include <libguile.h>
#include <cassert>
#include <stdexcept>

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

static scm_t_bits frame_tag;

static bool str_eqaul_sym(const char* str, SCM sym) {
  assert(scm_is_true(scm_symbol_p(sym)) == 1);
  return scm_is_true(scm_eq_p(scm_from_locale_symbol(str), sym)) == 1;
}

static common::Frame::Format scm_to_fmt(SCM value) {
  if (str_eqaul_sym("rgba", value)) {
    return common::Frame::RGBA;
  } else if (str_eqaul_sym("rgb", value)) {
    return common::Frame::RGB;
  }
  throw std::runtime_error("Unknown frame format");
}

SCM common::make_frame(SCM width, SCM height, SCM format) {
  auto place = scm_gc_malloc(sizeof(Frame), "frame");
  auto frame = new (place)
      Frame(scm_to_int(width), scm_to_int(height), scm_to_fmt(format));
  return scm_new_smob(frame_tag, (scm_t_bits)frame);
}

common::Frame& common::scm_to_frame(SCM frame) {
  scm_assert_smob_type(frame_tag, frame);
  return *reinterpret_cast<Frame*>(SCM_SMOB_DATA(frame));
}

extern "C" {
void scm_init_common_module() {
  scm_c_define_gsubr("hans-hash", 1, 1, 0, (scm_t_subr)common::hans_hash);

  frame_tag = scm_make_smob_type("frame", sizeof(common::Frame));
  scm_c_define_gsubr("make-frame", 3, 1, 0, (scm_t_subr)common::make_frame);
}
}
