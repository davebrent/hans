#ifndef HANS_COMMON_TYPES_H_
#define HANS_COMMON_TYPES_H_

#include <stddef.h>
#include <stdint.h>

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
    LIBRARIES,
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
}
}

#endif // HANS_COMMON_TYPES_H_
