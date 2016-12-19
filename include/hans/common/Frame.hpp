#ifndef HANS_COMMON_FRAME_H
#define HANS_COMMON_FRAME_H

#include <stdint.h>
#include <cstdlib>

namespace hans {
namespace common {

struct Frame {
  using Buffer = unsigned char*;
  enum Format { RGB, RGBA };

  Buffer buffer;
  Format format;

  uint16_t width;
  uint16_t height;

  Frame(uint16_t w, uint16_t h, Format fmt) {
    buffer = static_cast<Buffer>(
        std::calloc(w * h * (fmt == RGB ? 3 : 4), sizeof(char)));
    format = fmt;
    width = w;
    height = h;
  }

  ~Frame() {
    delete[] buffer;
  }

  uint16_t channels() const {
    return (format == RGB) ? 3 : 4;
  }
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_FRAME_H
