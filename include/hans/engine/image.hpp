#ifndef HANS_ENGINE_IMAGE_H_
#define HANS_ENGINE_IMAGE_H_

#include "hans/engine/primitives.hpp"

namespace hans {
namespace engine {
namespace image {

bool encode(const char* filepath, const Frame& frame);

} // namespace image
} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_IMAGE_H_
