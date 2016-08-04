#include "hans/graphics/Window.hpp"
#include <GLFW/glfw3.h>
#include <cassert>
#include <iostream>

using namespace hans;
using namespace hans::graphics;

static void error_callback(int error, const char* description) {
  std::cout << "Window Error: " << error << " " << description << std::endl;
}

Window::Window(const char* title, unsigned width, unsigned height) {
  m_window = nullptr;

  glfwSetErrorCallback(error_callback);
  bool res = glfwInit();
  assert(res == GL_TRUE);

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

  m_window = glfwCreateWindow(width, height, title, nullptr, nullptr);
  assert(m_window != nullptr);

  glfwMakeContextCurrent(m_window);
  glfwSwapInterval(1);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glClearColor(0, 0, 0, 1);
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
