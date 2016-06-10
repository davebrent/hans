#ifndef HANS_COMMON_TYPES_H_
#define HANS_COMMON_TYPES_H_

#include <stddef.h>
#include <stdint.h>

/// Unique ID for each object instance
typedef uint32_t hans_instance_id;

/// Hashed string
typedef uint64_t hans_hash;

// clang-format off
/// Possible object types
enum hans_object_type {
  HANS_OBJECT_AUDIO,
  HANS_OBJECT_GRAPHICS
};

// FIXME: The include ordering in this and the other types_* files
#include "./types_arguments.hpp"
#include "./types_registers.hpp"
#include "./types_parameters.hpp"
#include "./types_audio.hpp"
#include "./types_graphics.hpp"
#include "./types_resources.hpp"
#include "./types_module.hpp"
#include "./types_patcher.hpp"
// clang-format on

typedef struct {
  uint8_t channels;
  uint16_t samplerate;
  uint16_t blocksize;
  uint16_t width;
  uint16_t height;
} hans_config;

enum hans_blob_type {
  HANS_BLOB_STRINGS,
  HANS_BLOB_STRING_HASHES,
  HANS_BLOB_STRING_OFFSETS,
  HANS_BLOB_LIBRARIES,
  HANS_BLOB_OBJECTS,
  HANS_BLOB_OBJECTS_DATA,
  HANS_BLOB_PARAMETERS,
  HANS_BLOB_PARAMETER_VALUES,
  HANS_BLOB_PROGRAMS,
  HANS_BLOB_CHAINS,
  HANS_BLOB_REGISTERS,
  HANS_BLOB_RESOURCE_REQUESTS,
  HANS_BLOB_SHADERS,
  HANS_BLOB_FBOS,
  HANS_BLOB_FBO_ATTACHMENTS,
  HANS_BLOB_AUDIO_BUFFERS
};

typedef struct {
  // Type of data encoded in the blob
  hans_blob_type type;
  size_t offset;
  // Size in bytes of the data
  size_t size;
  void* data;
} hans_blob;

typedef struct {
  uint16_t version;
  bool little_endian;
  // Sum of data contained in blobs
  size_t size;
  // Number of blobs contained in the file
  size_t length;
  hans_blob* blobs;
  // The data referenced by blob.offset
  void* data;
} hans_file;

#endif // HANS_COMMON_TYPES_H_
