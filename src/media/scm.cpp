#include "hans/common/scm.hpp"
#include <libguile.h>
#include "./Image.hpp"

using namespace hans;

static SCM image_encode(SCM frame, SCM filepath) {
  auto path = scm_to_locale_string(filepath);
  auto result = media::image::encode(path, common::scm_to_frame(frame));
  std::free(path);
  return (result) ? SCM_BOOL_T : SCM_BOOL_F;
}

extern "C" {
void scm_init_hans_media_module() {
  scm_c_define_gsubr("image-encode", 2, 0, 0, (scm_t_subr)image_encode);
}
}
