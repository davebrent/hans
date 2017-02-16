#ifndef HANS_VIDEO_H_
#define HANS_VIDEO_H_

#include <cstring>
#include <fstream>
#include "hans/primitives.hpp"

namespace hans {

class VideoEncoder {
 public:
  VideoEncoder(const char* path, uint16_t width, uint16_t height);
  ~VideoEncoder();
  bool encode(const Frame& frame);
  bool close();

 private:
  std::ofstream m_stream;
  size_t m_capacity;
  unsigned char* m_frame;
  void* m_plane;
  unsigned char* m_temp_row;
  uint16_t m_width;
  uint16_t m_height;
};

} // namespace hans

#endif // HANS_VIDEO_H_
