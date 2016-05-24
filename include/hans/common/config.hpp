#ifndef HANS_COMMON_CONFIG_H_
#define HANS_COMMON_CONFIG_H_

#include <stdint.h>

/// Settings for the global audio device & stream
typedef struct {
  uint8_t num_channels;
  uint16_t sample_rate;
  uint16_t block_size;
} hans_audio_device_parameters;

/// Settings for the graphics window
typedef struct {
  uint16_t width;
  uint16_t height;
} hans_graphics_window_parameters;

/// Settings passed into main
typedef struct {
  hans_audio_device_parameters audio;
  hans_graphics_window_parameters window;
} hans_config;

#endif // HANS_COMMON_CONFIG_H_