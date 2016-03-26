#ifndef HANS_COMMON_TYPES_H_
#define HANS_COMMON_TYPES_H_

#include <stddef.h>
#include <stdint.h>

/// An ID representing a type of object
typedef uint16_t hans_object_id;

/// An ID representing an instance an object
typedef uint16_t hans_instance_id;

/// An ID representing a type of parameter
typedef uint16_t hans_parameter_id;

/// An ID representing a type of parameter
typedef uint16_t hans_resource_id;

/// Represents a hashed string
typedef uint64_t hans_hash;

/// Value type of a parameter
typedef float hans_parameter_value;

/// Size of a parameter
typedef uint8_t hans_parameter_size;

/// Value type of a parameter
typedef float hans_audio_sample;

typedef struct { uint16_t bin; } hans_register_handle;

enum hans_arg_type { HANS_BOOL, HANS_NUMBER, HANS_STRING };

enum hans_resource_type {
  HANS_PARAMETER,
  HANS_SHADER,
  HANS_AUDIO_BUFFER,
  HANS_FRAME_BUFFER,
  HANS_INLET,
  HANS_OUTLET
};

/// A key value object argument to create an object
typedef struct {
  hans_hash name;
  hans_arg_type type;
  union {
    int32_t number;
    bool boolean;
    hans_hash string;
  };
} hans_user_arg;

/// A key value object argument to create an object
typedef struct {
  int32_t length;
  hans_user_arg *data;
} hans_object_arguments;

/// A graph node
typedef struct {
  hans_object_id object_id;
  /// ID assigned by a program, is unique across graphs but not across programs
  hans_instance_id instance_id;
  /// Pointer to args
  hans_user_arg *arguments;
  /// Index into the graphs args array
  uint16_t arguments_index;
  /// Number of args this object node has
  uint16_t arguments_len;
  uint16_t inlets;
  uint16_t outlets;
} hans_user_object;

/// An edge to a graph, source and sink are indexes into a corresponding array
/// of nodes
typedef struct {
  /// The source objects array index
  uint16_t source;
  /// The source objects outlet number
  uint16_t outlet;
  /// The sink objects array index
  uint16_t sink;
  /// The sink objects inlet number
  uint16_t inlet;
} hans_object_connection;

/// A whole parameter with values attached, used for initializing instances of
/// the type of parameter
typedef struct {
  hans_object_id object_id;
  hans_parameter_id id;
  hans_hash name;
  hans_parameter_size size;
  hans_parameter_value *values;
} hans_parameter;

/// A handle to a parameter
typedef struct {
  /// Base index of the parameter in the values array
  uint16_t value;
  /// Index into arrays describing the parameter
  uint16_t meta;
} hans_parameter_handle;

/// Functions to help reduce the tedium of creating object instances and used as
/// a level of indirection between the users JSON representation of the
/// execution graph & the objects themselves, help in creating a standalone
/// program from an object???
typedef struct hans_constructor_api hans_constructor_api;
struct hans_constructor_api {
  /// Return a pointer to the start of the objects arguments array
  hans_object_arguments (*get_args)(hans_constructor_api *api);

  /// Signal to the engine/callee that the object is missing a required argument
  void (*missing_required_arg)(hans_constructor_api *api, hans_hash name,
                               hans_arg_type type);

  /// Request a number of resources to be passed into the objects setup method
  void (*request_resource)(hans_constructor_api *api, hans_resource_type type,
                           int32_t amount);
  void *data;
};

/// Create and delete an instance of a hans object
typedef void (*hans_new_object)(hans_constructor_api *api, void *buffer,
                                size_t size);
typedef void (*hans_del_object)(void *instance);

enum hans_object_type { HANS_AUDIO, HANS_GRAPHICS, HANS_UNKOWN };

/// A hans object, that when created may produce pixels OR sound
typedef struct {
  /// Unique ID of instances of the object
  hans_object_id id;
  /// Type of object
  hans_object_type type;
  /// Hash of the human readable name of the object
  hans_hash name;
  /// The size in bytes of an instance of the object
  size_t size;
  /// Create a new instance of the object
  hans_new_object make;
  /// Destroy an instance of the object
  hans_del_object destroy;
} hans_object;

/// A hans library to be loaded as a plugin
typedef struct {
  /// A hash of the human readable name of the library
  hans_hash name;
  /// The filepath of the library
  hans_hash filepath;
  /// The libraries open handle
  void *handle;
} hans_library;

/// Supported shader types
enum hans_shader_type { HANS_SHADER_VERTEX, HANS_SHADER_FRAGMENT };

/// A shader
typedef struct {
  hans_shader_type type;
  hans_hash uri;
  hans_hash code;
  hans_resource_id resource_id;
} hans_shader;

typedef uint16_t hans_shader_handle;
typedef int16_t hans_shader_program_handle;

typedef struct {
  hans_shader_handle handle;
  hans_hash uri;
} hans_shader_instance;

typedef struct {
  hans_shader_program_handle handle;
} hans_shader_program_instance;

/// hans plugin api for external C libraries
typedef struct hans_library_api hans_library_api;
struct hans_library_api {
  /// Register a new object type
  bool (*register_object)(hans_library_api *api, const char *name, size_t size,
                          hans_new_object new_instance,
                          hans_del_object del_instance);
  void *data;
};

/// Entry function for C libraries
typedef void (*hans_module_setup)(hans_library_api *);

typedef uint8_t hans_audio_device_id;

/// An I/O device for audio
typedef struct {
  hans_audio_device_id id;
  const char *name;
  uint8_t max_input_channels;
  uint8_t max_output_channels;
  bool default_input;
  bool default_output;
} hans_audio_device;

typedef struct {
  uint8_t channels;
  uint64_t samples_len;
  hans_audio_sample **samples;
} hans_audio_buffer;

typedef uint16_t hans_audio_bus_handle;

typedef uint16_t hans_fbo_id;

enum hans_fbo_attachment_type {
  HANS_COLOR_ATTACHMENT,
  HANS_DEPTH_ATTACHMENT,
  HANS_STENCIL_ATTACHMENT
};

typedef struct {
  hans_fbo_id fbo_id;
  hans_fbo_attachment_type type;
  uint32_t index;
  uint32_t width;
  uint32_t height;
  uint32_t components;
} hans_fbo_attachment;

typedef struct {
  hans_fbo_id id;
  hans_object_id object_id;
  bool stencil_buffer;
  hans_fbo_attachment *attachments;
  uint32_t num_attachments;
} hans_fbo;

typedef struct {
  uint32_t fbo_index;
  uint32_t attachments_index;
  uint32_t num_color_attachments;
  bool has_depth;
  bool has_stencil;
} hans_fbo_handle;

/// A key value object argument to create an object
typedef struct {
  hans_hash name;
  hans_resource_type type;
  union {
    hans_parameter_handle parameter;
    hans_shader_instance shader;
    hans_fbo_handle frame_buffer;
    hans_audio_buffer *audio_buffer;
    hans_register_handle inlet;
    hans_register_handle outlet;
  };
} hans_object_resource;

#endif // HANS_COMMON_TYPES_H_
