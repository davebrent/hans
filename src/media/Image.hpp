#ifndef HANS_MEDIA_IMAGE_H_
#define HANS_MEDIA_IMAGE_H_

#include "hans/common/Frame.hpp"

namespace hans {
namespace media {
namespace image {

bool encode(const char* filepath, const common::Frame& frame);

} // namespace image
} // namespace media
} // namespace hans

#endif // HANS_MEDIA_MEDIAENCODER_H_
