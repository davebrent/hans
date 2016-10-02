#ifndef HANS_GRAPHICS_WINDOW_H_
#define HANS_GRAPHICS_WINDOW_H_

#include "hans/graphics/gl.h"

struct GLFWwindow;

namespace hans {
namespace graphics {

class Window {
 public:
  Window();
  ~Window();
  bool make(const char* title, uint16_t width, uint16_t height);
  bool should_close();
  void update();

 private:
  GLFWwindow* m_window;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_WINDOW_H_
