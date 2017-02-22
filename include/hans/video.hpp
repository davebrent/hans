#ifndef HANS_VIDEO_H_
#define HANS_VIDEO_H_

#include <vpx/vpx_encoder.h>
#include <ostream>
#include "hans/primitives.hpp"

namespace hans {

class VideoEncoder {
 public:
  VideoEncoder(std::ostream& os, size_t frames, uint16_t width,
               uint16_t height);
  ~VideoEncoder();
  void encode(const Frame& frame);

 private:
  size_t _frameno;
  std::ostream& _os;
  vpx_image_t _img;
  vpx_codec_ctx_t _codec;
  unsigned char* _temp_row;
};

} // namespace hans

#endif // HANS_VIDEO_H_
