#include "hans/graphics/Window.hpp"
#include "hans/graphics/gl.h"
#include <GLFW/glfw3.h>
#include <cassert>

using namespace hans;

graphics::Window::Window(const char* title, unsigned width, unsigned height) {
  m_window = nullptr;

  bool res = glfwInit();
  assert(res == GL_TRUE);

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

  // glfwWindowHint(GLFW_FOCUSED, GL_FALSE);
  // glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
  // glfwWindowHint(GLFW_DECORATED, GL_FALSE);

  m_window = glfwCreateWindow(width, height, title, nullptr, nullptr);
  assert(m_window != nullptr);

  glfwMakeContextCurrent(m_window);
  glfwSwapInterval(1);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glClearColor(0, 0, 0, 1);
}

graphics::Window::~Window() {
  glfwDestroyWindow(m_window);
  glfwTerminate();
}

bool graphics::Window::should_close() {
  return glfwWindowShouldClose(m_window);
}

void graphics::Window::update() {
  glfwSwapBuffers(m_window);
  glfwPollEvents();
}
