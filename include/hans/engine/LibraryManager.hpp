#ifndef HANS_LIBRARYMANAGER_H_
#define HANS_LIBRARYMANAGER_H_

#include <vector>
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class LibraryManager {
 public:
  LibraryManager(hans::common::StringManager& string_manager,
                 std::vector<hans_object>& objects);
  ~LibraryManager();

  void load_libraries(std::vector<hans_library>& libraries);
  bool register_object(const char* name, size_t size, hans_new_object make,
                       hans_del_object destroy);

  std::vector<hans_object> filter_objects(hans_object_type type);

 private:
  hans::common::StringManager& m_string_manager;
  std::vector<void*> m_handles;
  std::vector<hans_object>& m_objects;
};

} // namespace engine
} // namespace hans

#endif // HANS_LIBRARYMANAGER_H_
