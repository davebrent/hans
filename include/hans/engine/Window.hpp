#ifndef HANS_ENGINE_WINDOW_H_
#define HANS_ENGINE_WINDOW_H_

#include <stdint.h>

struct GLFWwindow;

namespace hans {
namespace engine {

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

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_WINDOW_H_
