#ifndef HANS_COMMON_TYPES_PATCHER_H_
#define HANS_COMMON_TYPES_PATCHER_H_

#include <stdint.h>

typedef struct {
  hans_instance_id id;
  hans_object_type type;
  hans_hash name;
  size_t size;
  hans_new_object make;
  hans_init_object init;
  hans_del_object destroy;
  void* instance;
} hans_object;

typedef struct {
  hans_instance_id id;
  size_t start;
  size_t end;
} hans_chain;

typedef struct {
  hans_hash name;
  hans_chain graphics;
  hans_chain audio;
} hans_program;

#endif // HANS_COMMON_TYPES_PATCHER_H_
