#include "hans/graphics_debug.hpp"
#include <cstring>
#include <iostream>
#include "hans/gl.hpp"

using namespace hans;

GraphicsDebug::GraphicsDebug(const StringManager& strings, bool enabled)
    : _strings(strings), _enabled(enabled) {
}

void GraphicsDebug::push(const hash name) const {
  if (!_enabled) {
    return;
  }

  auto string = _strings.lookup(name);
  push(string);
}

void GraphicsDebug::push(const char* name) const {
  if (!_enabled) {
    return;
  }

  glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 0, strlen(name), name);
}

void GraphicsDebug::pop() const {
  if (!_enabled) {
    return;
  }

  glPopDebugGroup();
  // check OpenGL error
  GLenum err;
  while ((err = glGetError()) != GL_NO_ERROR) {
    std::cerr << "[HANS] OpenGL error: " << err << std::endl;
  }
}
