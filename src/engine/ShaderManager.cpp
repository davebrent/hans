#include "hans/engine/ShaderManager.hpp"
#include <iostream>
#include <stdexcept>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

ShaderManager::ShaderManager(const StringManager& string_manager,
                             const std::vector<Shader>& shaders)
    : m_string_manager(string_manager), m_shaders(shaders) {
}

void ShaderManager::destroy() {
  for (auto& shader : m_shader_handles) {
    glDeleteShader(shader);
  }
  for (auto& program : m_program_handles) {
    glDeleteProgram(program);
  }
}

const char* ShaderManager::validate(const hash name) {
  auto instance = create(name);
  auto handle = instance.handle;

  GLint status = GL_TRUE;
  glGetShaderiv(handle, GL_COMPILE_STATUS, &status);

  if (status != GL_TRUE) {
    GLint length = 0;
    glGetShaderiv(handle, GL_INFO_LOG_LENGTH, &length);

    auto message = new char[length + 1];
    glGetShaderInfoLog(handle, length, nullptr, static_cast<GLchar*>(message));
    message[length] = '\0';
    return message;
  }

  return nullptr;
}

Shader::Instance ShaderManager::create(const hash name) {
  Shader::Instance instance;
  instance.name = name;

  auto found = false;

  for (const auto& shader : m_shaders) {
    if (shader.name != name) {
      continue;
    }

    found = true;
    auto code = m_string_manager.lookup(shader.code);

    switch (shader.type) {
    case Shader::Types::VERTEX:
      instance.handle = glCreateShader(GL_VERTEX_SHADER);
      break;
    case Shader::Types::FRAGMENT:
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

ShaderProgram ShaderManager::create(const Shader::Instance& vertex,
                                    const Shader::Instance& fragment) {
  ShaderProgram program;
  program.handle = glCreateProgram();
  m_program_handles.push_back(program.handle);

  glAttachShader(program.handle, vertex.handle);
  glAttachShader(program.handle, fragment.handle);
  glLinkProgram(program.handle);
  return program;
}
