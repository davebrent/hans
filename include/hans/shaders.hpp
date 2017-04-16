#ifndef HANS_SHADERS_H_
#define HANS_SHADERS_H_

#include <vector>
#include "hans/gl.hpp"
#include "hans/primitives.hpp"
#include "hans/strings.hpp"

namespace hans {

class ShaderManager {
 public:
  ShaderManager(const ShaderManager& other) = delete;
  ShaderManager(const StringManager& string_manager,
                const std::vector<graphics::Shader>& shaders);
  void destroy();

  /// Returns nullptr if the shader is valid, other wise an error string that
  /// must be freed by the caller
  const char* validate(const hash uri);

  graphics::Shader::Instance create(const hash uri);
  graphics::ShaderProgram create(const graphics::Shader::Instance& geometry,
                                 const graphics::Shader::Instance& vertex,
                                 const graphics::Shader::Instance& fragment);
  graphics::ShaderProgram create(const graphics::Shader::Instance& vertex,
                                 const graphics::Shader::Instance& fragment);

 private:
  const StringManager& m_string_manager;
  const std::vector<graphics::Shader>& m_shaders;

  std::vector<GLuint> m_shader_handles;
  std::vector<GLuint> m_program_handles;
};

} // namespace hans

#endif // HANS_SHADERS_H_
