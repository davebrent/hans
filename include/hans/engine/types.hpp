#ifndef HANS_ENGINE_TYPES_H_
#define HANS_ENGINE_TYPES_H_

#include <stdint.h>
#include <functional>
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

struct Library {
  hash filepath;
};

struct ObjectDef {
  enum Types { AUDIO, GRAPHICS };
  typedef uint32_t ID;
  typedef std::function<void*(ID, void*)> Create;
  typedef std::function<void(void*)> Destroy;
  typedef std::function<void*(void*)> Serialize;
  ID id;
  Types type;
  hash name;
  size_t size;
  Create create;
  Destroy destroy;
  Serialize serialize;
  void* instance;
};

struct Chain {
  uint32_t id;
  size_t start;
  size_t end;
};

struct Program {
  hash name;
  Chain graphics;
  Chain audio;
};

struct Register {
  enum Types { INLET, OUTLET };
  ObjectDef::ID object;
  ObjectDef::Types type;
  uint32_t graph;
  uint8_t index;
  uint16_t bin;
  bool readonly;
};

struct Argument {
  enum Types { BOOLEAN, NUMBER, STRING };
  hash name;
  Types type;
  union {
    float number;
    bool boolean;
    hash string;
  };
};

struct Parameter {
  typedef float Value;
  typedef uint8_t Length;
  ObjectDef::ID object;
  hash name;
  Length size;
  size_t offset;
};

struct Modulator {
  struct Port {
    ObjectDef::ID object;
    hash parameter;
    uint8_t component;
  };

  Port source;
  Port dest;
  float offset;
  float scale;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_TYPES_H_
