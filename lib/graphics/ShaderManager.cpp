#include "hans/graphics/ShaderManager.hpp"
#include <stdexcept>
#include <iostream>

using namespace hans;

graphics::ShaderManager::ShaderManager(
    const common::StringManager& string_manager,
    const hans::common::ListView<hans_shader>& shaders)
    : m_string_manager(string_manager) {
  m_shaders = &shaders[0];
  m_length = shaders.size();
}

graphics::ShaderManager::~ShaderManager() {
  for (auto& shader : m_shader_handles) {
    glDeleteShader(shader);
  }
  for (auto& program : m_program_handles) {
    glDeleteProgram(program);
  }
}

const char* graphics::ShaderManager::validate_shader(const hans_hash name) {
  auto instance = create_shader(name);
  auto handle = instance.handle;

  GLint status = GL_TRUE;
  glGetShaderiv(handle, GL_COMPILE_STATUS, &status);

  if (status != GL_TRUE) {
    GLint length = 0;
    glGetShaderiv(handle, GL_INFO_LOG_LENGTH, &length);

    char* message = new char[length + 1];
    glGetShaderInfoLog(handle, length, nullptr, static_cast<GLchar*>(message));
    message[length] = '\0';
    return message;
  }

  return nullptr;
}

hans_shader_instance graphics::ShaderManager::create_shader(
    const hans_hash name) {
  hans_shader_instance instance;
  instance.name = name;

  auto shaders = m_shaders;
  auto found = false;

  for (auto i = 0; i < m_length; ++i) {
    auto& shader = shaders[i];
    if (shader.name != name) {
      continue;
    }

    found = true;
    auto code = m_string_manager.lookup(shader.code);

    switch (shader.type) {
    case HANS_SHADER_VERTEX:
      instance.handle = glCreateShader(GL_VERTEX_SHADER);
      break;
    case HANS_SHADER_FRAGMENT:
      instance.handle = glCreateShader(GL_FRAGMENT_SHADER);
      break;
    default:
      throw std::runtime_error("Shader not recognised");
    }

    glShaderSource(instance.handle, 1, &code, nullptr);
    break;
  }

  if (!found) {
    throw std::runtime_error("Shader not found");
  }

  m_shader_handles.push_back(instance.handle);
  glCompileShader(instance.handle);
  return instance;
}

hans_shader_program_instance graphics::ShaderManager::create_program(
    const hans_shader_instance& vertex_shader,
    const hans_shader_instance& fragment_shader) {
  hans_shader_program_instance program;
  program.handle = glCreateProgram();
  m_program_handles.push_back(program.handle);

  glAttachShader(program.handle, vertex_shader.handle);
  glAttachShader(program.handle, fragment_shader.handle);
  glLinkProgram(program.handle);
  return program;
}
