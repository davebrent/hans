#ifndef HANS_ENGINE_WINDOW_H_
#define HANS_ENGINE_WINDOW_H_

#include <stdint.h>
#include "hans/common/types.hpp"

struct GLFWwindow;

namespace hans {
namespace engine {

class Window {
 public:
  Window(const Window& other) = delete;
  Window();
  ~Window();
  bool make(const char* title, uint16_t width, uint16_t height);
  bool should_close();
  void update();
  void capture(Frame& frame);

 private:
  GLFWwindow* m_window;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_WINDOW_H_
