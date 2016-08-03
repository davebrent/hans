#ifndef HANS_SCRIPT_TYPES_H_
#define HANS_SCRIPT_TYPES_H_

#include <libguile.h>
#include "hans/common/types.hpp"

struct ScriptState {
  hans_hash path;
  hans_fbo fbo;
  hans_register outlet;
  float width;
  float height;
  SCM draw;
};

#endif // HANS_SCRIPT_TYPES_H_
