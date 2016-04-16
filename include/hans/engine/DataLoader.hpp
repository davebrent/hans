#ifndef HANS_DATALOADER_H_
#define HANS_DATALOADER_H_

#include <vector>
#include "hans/common/types.hpp"
#include "hans/memory/StringManager.hpp"

class sqlite3;

namespace hans {
namespace engine {

class DataLoader {
 public:
  explicit DataLoader(const char* path,
                      hans::memory::StringManager& string_manager);
  ~DataLoader();

  /// Returns all libraries that should be loaded
  std::vector<hans_library> get_libraries() const;

  /// Returns all objects that are expected to be found
  std::vector<hans_object> get_objects() const;

  /// Returns all objects that are expected to be found
  std::vector<hans_parameter> get_parameters() const;
  void del_parameters(std::vector<hans_parameter>& parameters) const;

  /// Returns all available shaders
  std::vector<hans_shader> get_shaders() const;

  /// Returns all frame buffer configurations
  std::vector<hans_fbo> get_frame_buffers() const;
  void del_frame_buffers(std::vector<hans_fbo>& frame_buffers) const;

 private:
  sqlite3* m_connection;
  hans::memory::StringManager& m_string_manager;
};

} // namespace engine
} // namespace hans

#endif // HANS_DATALOADER_H_
