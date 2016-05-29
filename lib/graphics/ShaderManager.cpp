#include "hans/graphics/ShaderManager.hpp"
#include <algorithm>

using namespace hans;

graphics::ShaderManager::ShaderManager(
    common::Logger& logger, const common::StringManager& string_manager,
    const std::vector<hans_shader>& shaders)
    : m_logger(logger), m_string_manager(string_manager), m_shaders(shaders) {
}

graphics::ShaderManager::~ShaderManager() {
  for (auto& shader : m_shader_handles) {
    glDeleteShader(shader);
  }
  for (auto& program : m_program_handles) {
    glDeleteProgram(program);
  }
}

int graphics::ShaderManager::make(hans_object_resource* resources, int len) {
  for (int a = 0; a < len; ++a) {
    resources->type = HANS_SHADER;
    resources++;
  }
  return len;
}

bool graphics::ShaderManager::create_shader(hans_shader_instance& instance,
                                            hans_hash uri) {
  auto end = m_shaders.end();
  auto it = std::find_if(m_shaders.begin(), end,
                         [&](const auto& shader) { return shader.uri == uri; });
  if (it == end) {
    m_logger.log(common::Logger::ERROR, "Shader not found");
    return false;
  }

  auto shader = *it;
  const char* code = m_string_manager.lookup(shader.code);

  instance.uri = uri;

  switch (shader.type) {
  case HANS_SHADER_VERTEX:
    instance.handle = glCreateShader(GL_VERTEX_SHADER);
    break;
  case HANS_SHADER_FRAGMENT:
    instance.handle = glCreateShader(GL_FRAGMENT_SHADER);
    break;
  }

  m_shader_handles.push_back(instance.handle);

  glShaderSource(instance.handle, 1, &code, nullptr);
  glCompileShader(instance.handle);

  GLint length = 0;
  GLint status = GL_FALSE;
  glGetShaderiv(instance.handle, GL_COMPILE_STATUS, &status);

  if (status != GL_TRUE) {
    glGetShaderiv(instance.handle, GL_INFO_LOG_LENGTH, &length);
    char* reason = new char[length + 1];
    glGetShaderInfoLog(instance.handle, length, nullptr,
                       static_cast<GLchar*>(reason));
    m_logger.log(common::Logger::ERROR, reason);
    return false;
  }

  return true;
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
