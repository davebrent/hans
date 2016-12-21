#include "hans/common/scm.hpp"
#include <libguile.h>
#include <new>
#include "./Image.hpp"
#include "./Video.hpp"
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"

using namespace hans;
using namespace hans::media;

static SCM image_encode(SCM frame, SCM filepath) {
  auto path = scm_to_locale_string(filepath);
  auto result = media::image::encode(path, scm::to_cpp<Frame>(frame));
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
  auto& encoder = scm::to_cpp<VideoEncoder>(scm_encoder);
  encoder.close();
  return SCM_BOOL_T;
}

extern "C" {
void scm_init_hans_media_module() {
  scm::procedure<image_encode>("image-encode", 2, 0, 0);

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
}
}
