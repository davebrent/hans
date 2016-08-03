#ifndef HANS_COMMON_TYPES_MODULE_H_
#define HANS_COMMON_TYPES_MODULE_H_

#include <stdint.h>
#include <functional>

typedef struct { hans_hash filepath; } hans_library;

class ObjectPatcher {
 public:
  virtual hans_arguments get_args() = 0;
  virtual void missing_arg(hans_argument_type type, hans_hash name) = 0;
  virtual void request(hans_resource_type type, size_t value) = 0;
};

typedef std::function<void*(hans_instance_id, void*)> hans_new_object;
typedef std::function<void(void*)> hans_del_object;
typedef std::function<void*(void*)> hans_serialize_object;

namespace hans {
namespace engine {
class LibraryManager;
}
}

typedef void (*hans_module_setup)(hans::engine::LibraryManager*);

#endif // HANS_COMMON_TYPES_MODULE_H_
