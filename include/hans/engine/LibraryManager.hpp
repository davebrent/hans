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
                 hans::common::ListView<hans_object>& objects);
  ~LibraryManager();
  /// Load the libraries
  void load(const common::ListView<hans_library>& libraries);
  /// Declare an object
  bool declare(const char* name, size_t size, hans_new_object make,
               hans_init_object init, hans_del_object destroy);

 private:
  hans::common::StringManager& m_string_manager;
  std::vector<void*> m_handles;
  hans_object* m_objects;
  size_t m_length;
};

} // namespace engine
} // namespace hans

#endif // HANS_LIBRARYMANAGER_H_
