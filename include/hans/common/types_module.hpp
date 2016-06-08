#ifndef HANS_COMMON_TYPES_MODULE_H_
#define HANS_COMMON_TYPES_MODULE_H_

#include <stdint.h>

typedef struct { hans_hash filepath; } hans_library;

typedef struct hans_constructor_api hans_constructor_api;
struct hans_constructor_api {
  hans_arguments (*get_arguments)(hans_constructor_api* api);
  void (*missing_argument)(hans_constructor_api* api, hans_argument_type type,
                           hans_hash name);
  void (*request_resource)(hans_constructor_api* api, hans_resource_type type,
                           int32_t amount);
  void* data;
};

typedef void (*hans_new_object)(hans_constructor_api* api, void* data,
                                size_t size);
typedef void (*hans_init_object)(void* instance);
typedef void (*hans_del_object)(void* instance);

typedef struct hans_library_api hans_library_api;
struct hans_library_api {
  bool (*register_object)(hans_library_api* api, const char* name, size_t size,
                          hans_new_object new_instance,
                          hans_init_object init_instance,
                          hans_del_object del_instance);
  void* data;
};

typedef void (*hans_module_setup)(hans_library_api*);

#endif // HANS_COMMON_TYPES_MODULE_H_
