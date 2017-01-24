#ifndef HANS_COMMON_PRIMITIVES_H_
#define HANS_COMMON_PRIMITIVES_H_

#include <stddef.h>
#include <stdint.h>
#include <cstdlib>
#include <string>
#include <vector>

namespace hans {

using hash = uint64_t;

struct Settings {
  uint16_t channels;
  uint16_t samplerate;
  uint16_t blocksize;
  uint16_t width;
  uint16_t height;
};

struct Blob {
  size_t size;
  void* data;
};

struct ObjectDef {
  enum Types { AUDIO, GRAPHICS };
  using ID = uint32_t;
  ID id;
  hash name;
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
  // XXX: Not a union type so serialization works as expected
  float number;
  bool boolean;
  hash string;
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
  size_t index;
};

struct Frame {
  using Buffer = unsigned char*;
  Buffer buffer;

  uint16_t width;
  uint16_t height;

  Frame(uint16_t w, uint16_t h) {
    auto channels = 4; // Frame data is expected to be BGRA
    buffer = static_cast<Buffer>(std::calloc(w * h * channels, sizeof(char)));
    width = w;
    height = h;
  }

  ~Frame() {
    std::free(buffer);
  }
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

struct Strings {
  std::string buffer;
  std::vector<hash> hashes;
  std::vector<size_t> lengths;
};

struct Arguments {
  std::vector<Argument> arguments;
  std::vector<size_t> lengths;
  std::vector<size_t> offsets;
};

struct Recordings {
  std::vector<size_t> offsets;
  std::vector<size_t> lengths;
  std::vector<Parameter::Value> values;
};

struct Range {
  size_t start;
  size_t end;
};

struct Graphs {
  std::vector<ObjectDef> objects;
  std::vector<std::string> states;
  std::vector<size_t> indices;
  std::vector<Range> ranges;
};

struct Plugins {
  std::vector<hash> filepaths;
};

struct Programs {
  std::vector<hash> names;
  Graphs audio;
  Graphs graphics;
};

struct EngineData {
  Settings settings;
  Strings strings;
  Plugins plugins;
  Programs programs;
  std::vector<Parameter> parameters;
  std::vector<Parameter::Value> parameters_values;
  std::vector<Modulator> modulators;
  std::vector<Register> registers;
  std::vector<RingBuffer> ring_buffers;
  std::vector<graphics::Shader> shaders;
  std::vector<graphics::FBO> fbos;
  std::vector<graphics::FBO::Attachment> fbos_attachments;
  std::vector<audio::Buffer> audio_buffers;
  Recordings recordings;
};
} // namespace hans

#endif // HANS_COMMON_PRIMITIVES_H_
