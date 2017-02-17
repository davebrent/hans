#ifndef HANS_USERCONFIG_H_
#define HANS_USERCONFIG_H_

#include <string>
#include <vector>
#include "hans/primitives.hpp"

namespace hans {

struct user_arg {
  std::string name;
  Argument::Types type;
  std::string string;
  double number;
  bool boolean;
};

struct user_parameter {
  std::string name;
  std::vector<double> value;
};

struct user_shader {
  std::string name;
  std::string type;
  std::string path;
  std::string code;
};

struct user_fbo_attachment {
  std::string type;
  size_t components;
  size_t width = 0;
  size_t height = 0;
};

struct user_fbo {
  bool stencil;
  std::vector<user_fbo_attachment> attachments;
};

struct user_audio_buffer {
  std::string name;
  size_t channels = 0;
  size_t size = 0;
};

struct user_object {
  std::string program;
  std::string name; // variable
  std::string type; // snd-?, gfx-?
  std::vector<user_arg> arguments;
};

struct user_connection {
  std::string source;
  size_t outlet;
  std::string target;
  size_t inlet;
};

struct user_modulation_port {
  std::string object;
  std::string parameter;
  size_t component;
};

struct user_modulator {
  user_modulation_port source;
  user_modulation_port target;
  double scale;
  double offset;
};

struct user_track {
  std::string sequence;
  float scale;
  user_modulation_port target;
};

struct user_program {
  std::string name;
  std::vector<user_object> objects;
  std::vector<user_connection> audio;
  std::vector<user_connection> graphics;
  std::vector<user_modulator> modulators;
  std::vector<user_track> tracks;
};

struct user_object_template {
  std::string name; // snd-?, gfx-?
  std::string type; // audio,graphics
  std::vector<user_parameter> parameters;
  std::vector<user_audio_buffer> audio_buffers;
  std::vector<user_fbo> frame_buffers;
  std::vector<user_shader> shaders;
};

struct user_plugin {
  std::string basepath;
  std::string infopath;
  std::string name;
  std::vector<user_object_template> objects;
};

struct user_reload {
  size_t delay;
  std::vector<std::string> paths;
  std::vector<std::string> extensions;
  std::vector<std::string> exclude;
};

using user_settings = Settings;

struct user_data {
  user_settings settings;
  std::vector<user_plugin> plugins;
  std::vector<user_program> programs;
  std::vector<user_object> objects;
  user_reload reload;
};

} // hans

#endif // HANS_USERCONFIG_H_
