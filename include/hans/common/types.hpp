#ifndef HANS_COMMON_TYPES_H_
#define HANS_COMMON_TYPES_H_

#include <stddef.h>
#include <stdint.h>
#include <functional>

namespace hans {

typedef uint64_t hash;

namespace common {

struct Config {
  uint8_t channels;
  uint16_t samplerate;
  uint16_t blocksize;
  uint16_t width;
  uint16_t height;
};

struct DataFile {
  enum Types {
    STRINGS,
    STRING_HASHES,
    STRING_OFFSETS,
    PLUGINS,
    OBJECTS,
    OBJECTS_DATA,
    PARAMETERS,
    PARAMETER_VALUES,
    PROGRAMS,
    CHAINS,
    REGISTERS,
    SHADERS,
    FBOS,
    FBO_ATTACHMENTS,
    AUDIO_BUFFERS,
    RING_BUFFERS,
    MODULATORS
  };

  struct Blob {
    Types type;
    size_t offset;
    size_t size;
    void* data;
  };

  uint16_t version;
  bool little_endian;
  size_t size;
  size_t length;
  Blob* blobs;
  void* data;
};
} // namespace common

struct Plugin {
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

struct RingBuffer {
  ObjectDef::ID producer;
  hash name;
  size_t offset;
  size_t index;
};

namespace audio {
typedef float sample;

struct Buffer {
  ObjectDef::ID object;
  hash name;
  uint8_t channels;
  uint64_t size;
  size_t offset;
};

struct Device {
  typedef uint8_t ID;
  ID id;
  const char* name;
  uint8_t max_input_channels;
  uint8_t max_output_channels;
  bool default_input;
  bool default_output;
};

typedef size_t bus_handle;
} // namespace audio

namespace graphics {
struct Shader {
  typedef uint16_t Handle;
  enum Types { VERTEX, FRAGMENT };
  Types type;
  hash name;
  hash code;

  struct Instance {
    Handle handle;
    hash name;
  };
};

struct ShaderProgram {
  typedef int16_t Handle;
  Handle handle;
};

struct FBO {
  struct Attachment {
    enum Types { COLOR, DEPTH, STENCIL };

    Types type;
    uint32_t width;
    uint32_t height;
    uint32_t components;
  };

  ObjectDef::ID object;
  bool stencil_buffer;
  size_t start;
  size_t end;
};
} // namespace graphics
} // namespace hans

#endif // HANS_COMMON_TYPES_H_
