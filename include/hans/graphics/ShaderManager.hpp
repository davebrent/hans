#ifndef HANS_GRAPHICS_SHADERMANAGER_H_
#define HANS_GRAPHICS_SHADERMANAGER_H_

#include <vector>
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"
#include "hans/graphics/gl.h"
#include "hans/graphics/types.hpp"

namespace hans {
namespace graphics {

class ShaderManager {
 public:
  ShaderManager(const common::StringManager& string_manager,
                const common::ListView<Shader>& shaders);
  ~ShaderManager();

  /// Returns nullptr if the shader is valid, other wise an error string that
  /// must be freed by the caller
  const char* validate(const hash uri);

  Shader::Instance create(const hash uri);
  ShaderProgram create(const Shader::Instance& vertex,
                       const Shader::Instance& fragment);

 private:
  const common::StringManager& m_string_manager;
  const Shader* m_shaders;
  size_t m_length;

  std::vector<GLuint> m_shader_handles;
  std::vector<GLuint> m_program_handles;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_SHADERMANAGER_H_
