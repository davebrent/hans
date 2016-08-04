#ifndef HANS_GRAPHICS_TYPES_H_
#define HANS_GRAPHICS_TYPES_H_

#include <stdint.h>
#include "hans/common/types.hpp"

namespace hans {
namespace graphics {

struct Shader {
  typedef uint16_t Handle;
  enum Types { VERTEX, FRAGMENT };
  Types type;
  hash name;
  hash code;

  struct Instance {
    Handle handle;
    hash name;
  };
};

struct ShaderProgram {
  typedef int16_t Handle;
  Handle handle;
};

struct FBO {
  struct Attachment {
    enum Types { COLOR, DEPTH, STENCIL };

    Types type;
    uint32_t width;
    uint32_t height;
    uint32_t components;
  };

  engine::ObjectDef::ID object;
  bool stencil_buffer;
  size_t start;
  size_t end;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_TYPES_H_
