#ifndef HANS_SCRIPT_TYPES_H_
#define HANS_SCRIPT_TYPES_H_

#include <libguile.h>
#include "hans/common/types.hpp"

struct ScriptState {
  hans::hash path;
  hans::graphics::FBO fbo;
  hans::Register outlet;
  float width;
  float height;
  SCM draw;
};

#endif // HANS_SCRIPT_TYPES_H_
