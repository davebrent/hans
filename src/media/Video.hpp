#ifndef HANS_MEDIA_VIDEOENCODER_H_
#define HANS_MEDIA_VIDEOENCODER_H_

#include <fstream>
#include "hans/common/serialize.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace media {

class VideoEncoder {
 public:
  VideoEncoder(const char* path, uint16_t width, uint16_t height);
  ~VideoEncoder();
  bool encode(const Frame& frame);
  bool close();

  template <class Archive>
  void serialize(Archive& ar) {
  }

 private:
  std::ofstream m_stream;
  size_t m_capacity;
  unsigned char* m_frame;
  void* m_plane;
  unsigned char* m_temp_row;
  uint16_t m_width;
  uint16_t m_height;
};

} // namespace sequencer
} // namespace hans

#endif // HANS_MEDIA_MEDIAENCODER_H_
