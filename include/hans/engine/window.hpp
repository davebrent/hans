#ifndef HANS_ENGINE_WINDOW_H_
#define HANS_ENGINE_WINDOW_H_

#include <stdint.h>
#include <functional>
#include "hans/engine/primitives.hpp"

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
  void update_wait();
  void capture(Frame& frame);
  void keycallback(int key);
  void set_key_controls(std::function<void(int)> callback);
  GLFWwindow* get();

 private:
  GLFWwindow* m_window;
  std::function<void(int)> m_key_controls;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_WINDOW_H_
