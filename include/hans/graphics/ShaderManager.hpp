#ifndef HANS_GRAPHICS_SHADERMANAGER_H_
#define HANS_GRAPHICS_SHADERMANAGER_H_

#include <vector>
#include "hans/common/Logger.hpp"
#include "hans/common/types.hpp"
#include "hans/graphics/gl.h"
#include "hans/memory/StringManager.hpp"

namespace hans {
namespace graphics {

class ShaderManager {
 public:
  ShaderManager(hans::common::Logger& logger,
                const hans::memory::StringManager& string_manager,
                const std::vector<hans_shader>& shaders);
  ~ShaderManager();

  bool create_shader(hans_shader_instance& instance, hans_hash uri);

  int make(hans_object_resource* resources, int len);

  hans_shader_program_instance create_program(
      const hans_shader_instance& vertex_shader,
      const hans_shader_instance& fragment_shader);

 private:
  hans::common::Logger& m_logger;
  const hans::memory::StringManager& m_string_manager;
  const std::vector<hans_shader>& m_shaders;
  std::vector<GLuint> m_shader_handles;
  std::vector<GLuint> m_program_handles;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_SHADERMANAGER_H_
