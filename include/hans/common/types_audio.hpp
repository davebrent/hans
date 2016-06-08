#ifndef HANS_COMMON_TYPES_AUDIO_H_
#define HANS_COMMON_TYPES_AUDIO_H_

#include <stdint.h>

/// Value type of a parameter
typedef float hans_audio_sample;

typedef uint8_t hans_audio_device_id;

/// An I/O device for audio
typedef struct {
  hans_audio_device_id id;
  const char* name;
  uint8_t max_input_channels;
  uint8_t max_output_channels;
  bool default_input;
  bool default_output;
} hans_audio_device;

typedef struct {
  uint8_t channels;
  uint64_t samples_len;
  hans_audio_sample** samples;
} hans_audio_buffer;

typedef uint16_t hans_audio_bus_handle;

#endif // HANS_COMMON_TYPES_AUDIO_H_
