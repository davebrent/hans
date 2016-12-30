#include "hans/common/scm.hpp"
#include <libguile.h>
#include "./Image.hpp"
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"

using namespace hans;

static SCM image_encode(SCM frame, SCM filepath) {
  auto path = scm_to_locale_string(filepath);
  auto result = media::image::encode(path, scm::to_cpp<Frame>(frame));
  std::free(path);
  return (result) ? SCM_BOOL_T : SCM_BOOL_F;
}

extern "C" {
void scm_init_hans_media_module() {
  scm::procedure<image_encode>("image-encode", 2, 0, 0);
}
}
