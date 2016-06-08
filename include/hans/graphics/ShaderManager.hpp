#ifndef HANS_GRAPHICS_SHADERMANAGER_H_
#define HANS_GRAPHICS_SHADERMANAGER_H_

#include <vector>
#include "hans/common/ListView.hpp"
#include "hans/common/Logger.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"
#include "hans/graphics/gl.h"

namespace hans {
namespace graphics {

class ShaderManager {
 public:
  ShaderManager(const hans::common::StringManager& string_manager,
                const hans::common::ListView<hans_shader>& shaders);
  ~ShaderManager();

  hans_shader_instance create_shader(const hans_hash uri);
  hans_shader_program_instance create_program(
      const hans_shader_instance& vertex_shader,
      const hans_shader_instance& fragment_shader);

 private:
  const hans::common::StringManager& m_string_manager;
  const hans_shader* m_shaders;
  size_t m_length;

  std::vector<GLuint> m_shader_handles;
  std::vector<GLuint> m_program_handles;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_SHADERMANAGER_H_
