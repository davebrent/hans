#include "hans/engine/Window.hpp"
#include <iostream>
#include "hans/engine/gl.hpp"

using namespace hans;
using namespace hans::engine;

static void error_callback(int error, const char* description) {
  std::cout << "Window Error: " << error << " " << description << std::endl;
}

Window::Window() {
  m_window = nullptr;
}

bool Window::make(const char* title, uint16_t width, uint16_t height) {
  if (m_window != nullptr) {
    return false;
  }

  glfwSetErrorCallback(error_callback);
  bool res = glfwInit();
  if (res == GL_FALSE) {
    return false;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

  m_window = glfwCreateWindow(width, height, title, nullptr, nullptr);
  if (m_window == nullptr) {
    return false;
  }

  glfwMakeContextCurrent(m_window);
  glfwSwapInterval(1);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glClearColor(0, 0, 0, 1);
  return true;
}

Window::~Window() {
  glfwDestroyWindow(m_window);
  glfwTerminate();
}

bool Window::should_close() {
  return glfwWindowShouldClose(m_window);
}

void Window::update() {
  glfwSwapBuffers(m_window);
  glfwPollEvents();
}

void Window::capture(Frame& f) {
  auto fmt = f.format == Frame::RGB ? GL_BGR : GL_BGRA;
  glReadPixels(0, 0, f.width, f.height, fmt, GL_UNSIGNED_BYTE, f.buffer);
}
