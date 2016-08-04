#ifndef HANS_AUDIO_TYPES_H_
#define HANS_AUDIO_TYPES_H_

#include <stdint.h>
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"

namespace hans {
namespace audio {

typedef float sample;

/// An I/O device for audio
struct Device {
  typedef uint8_t ID;
  ID id;
  const char* name;
  uint8_t max_input_channels;
  uint8_t max_output_channels;
  bool default_input;
  bool default_output;
};

struct Buffer {
  engine::ObjectDef::ID object;
  hash name;
  uint8_t channels;
  uint64_t size;
  size_t offset;
};

typedef size_t bus_handle;

struct RingBuffer {
  engine::ObjectDef::ID producer;
  hash name;
  size_t offset;
  size_t index;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_TYPES_H_
