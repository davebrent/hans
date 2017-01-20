#ifndef HANS_ENGINE_SHADERMANAGER_H_
#define HANS_ENGINE_SHADERMANAGER_H_

#include <vector>
#include "hans/common/StringManager.hpp"
#include "hans/common/primitives.hpp"
#include "hans/engine/gl.hpp"

namespace hans {
namespace engine {

class ShaderManager {
 public:
  ShaderManager(const ShaderManager& other) = delete;
  ShaderManager(const common::StringManager& string_manager,
                const std::vector<graphics::Shader>& shaders);
  void destroy();

  /// Returns nullptr if the shader is valid, other wise an error string that
  /// must be freed by the caller
  const char* validate(const hash uri);

  graphics::Shader::Instance create(const hash uri);
  graphics::ShaderProgram create(const graphics::Shader::Instance& vertex,
                                 const graphics::Shader::Instance& fragment);

 private:
  const common::StringManager& m_string_manager;
  const std::vector<graphics::Shader>& m_shaders;

  std::vector<GLuint> m_shader_handles;
  std::vector<GLuint> m_program_handles;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_SHADERMANAGER_H_
