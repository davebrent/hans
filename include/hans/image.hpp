#ifndef HANS_IMAGE_H_
#define HANS_IMAGE_H_

#include "hans/primitives.hpp"

namespace hans {
namespace image {

bool encode(const char* filepath, const Frame& frame);

} // namespace image
} // namespace hans

#endif // HANS_IMAGE_H_
