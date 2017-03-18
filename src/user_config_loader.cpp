#include "hans/user_config_loader.hpp"
#include <cpptoml.h>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include "hans/user_config.hpp"

using namespace hans;
using namespace std;

using table = cpptoml::table;

static bool read_settings(const shared_ptr<table>, user_data&);
static bool read_plugins(const shared_ptr<table>, user_data&);
static bool read_programs(const shared_ptr<table>, user_data&);
static bool read_watchers(const shared_ptr<table>, user_data&);

static bool read_parameters(const shared_ptr<table>, user_object_template&);
static bool read_shaders(const shared_ptr<table>, user_object_template&);
static bool read_audio_buffers(const shared_ptr<table>, user_object_template&);
static bool read_fbo(const shared_ptr<table>, user_object_template&);

static bool read_objects(const shared_ptr<table>, user_program&);
static bool read_audio(const shared_ptr<table>, user_program&);
static bool read_graphics(const shared_ptr<table>, user_program&);
static bool read_modulators(const shared_ptr<table>, user_program&);
static bool read_tracks(const shared_ptr<table>, user_program&);

using top_fn = function<bool(const shared_ptr<table>, user_data&)>;
using obj_fn = function<bool(const shared_ptr<table>, user_object_template&)>;
using pgm_fn = function<bool(const shared_ptr<table>, user_program&)>;

// clang-format off
static const top_fn top_tasks[] = {
  read_settings,
  read_plugins,
  read_programs,
  read_watchers,
};

static const obj_fn obj_tasks[] = {
  read_parameters,
  read_shaders,
  read_audio_buffers,
  read_fbo,
};

static const pgm_fn pgm_tasks[] = {
  read_objects,
  read_audio,
  read_graphics,
  read_modulators,
  read_tracks,
};
// clang-format on

static bool report(const std::string& err) {
  std::cerr << "[HANS] Pipeline::input: " << err << std::endl;
  return false;
}

static bool file_exists(const std::string filepath) {
  std::ifstream fs(filepath);
  return fs.good();
}

size_t required(const std::shared_ptr<cpptoml::table> input,
                const std::string& name, uint16_t* dest) {
  auto value = input->get_as<uint16_t>(name);

  if (!value) {
    std::ostringstream os;
    os << "Pipeline: Missing required setting '";
    os << name;
    os << "' Maybe the wrong type?";
    return report(os.str());
  }

  *dest = *value;
  return true;
}

static bool read_settings_graphics(const std::shared_ptr<cpptoml::table> input,
                                   user_gfx_settings& output) {
  auto settings = input->get_table("graphics");
  if (!settings) {
    return report("Missing audio settings table");
  }

  if (!required(settings, "width", &output.width)) {
    return false;
  }

  if (!required(settings, "height", &output.height)) {
    return false;
  }

  return true;
}

static bool read_settings_audio(const std::shared_ptr<cpptoml::table> input,
                                user_snd_settings& output) {
  auto settings = input->get_table("audio");

  output.backend = "";
  output.output_device = "";
  output.input_device = "";

  if (!required(settings, "blocksize", &output.blocksize)) {
    return false;
  }

  if (!required(settings, "samplerate", &output.samplerate)) {
    return false;
  }

  auto input_channels = settings->get_array_of<int64_t>("input_channels");
  if (!input_channels) {
    return report("Missing audio channel list");
  }

  for (const auto c : *input_channels) {
    output.input_channels.push_back(c);
  }

  auto output_channels = settings->get_array_of<int64_t>("output_channels");
  if (!output_channels) {
    return report("Missing audio channel list");
  }

  for (const auto c : *output_channels) {
    output.output_channels.push_back(c);
  }

  auto backend = settings->get_as<std::string>("backend");
  if (backend) {
    output.backend = *backend;
  }

  auto input_device = settings->get_as<std::string>("input_device");
  if (input_device) {
    output.input_device = *input_device;
  }

  auto output_device = settings->get_as<std::string>("output_device");
  if (output_device) {
    output.output_device = *output_device;
  }

  return true;
}

static bool read_settings(const std::shared_ptr<cpptoml::table> input,
                          user_data& output) {
  auto settings = input->get_table("settings");
  if (!settings) {
    return report("Missing settings table");
  }

  if (!read_settings_audio(settings, output.settings.audio)) {
    return false;
  }

  if (!read_settings_graphics(settings, output.settings.graphics)) {
    return false;
  }

  return true;
}

static bool read_shaders(const shared_ptr<table> input,
                         user_object_template& output) {
  auto shaders = input->get_table_array("shaders");
  if (!shaders) {
    return true;
  }

  for (const auto& shader : *shaders) {
    user_shader s;
    auto name = shader->get_as<std::string>("name");
    auto type = shader->get_as<std::string>("type");
    auto path = shader->get_as<std::string>("path");

    if (!name || !path || !type) {
      std::ostringstream ss;
      ss << "Pipeline: Invalid shader in ";
      ss << output.name << " ";
      ss << "'path', 'name' & 'type' are all required parameters";
      return report(ss.str());
    }

    s.name = *name;
    s.type = *type;
    s.path = *path;
    output.shaders.push_back(s);
  }

  return true;
}

static bool read_audio_buffers(const shared_ptr<table> input,
                               user_object_template& output) {
  auto buffers = input->get_table_array("audio-buffers");
  if (!buffers) {
    return true;
  }

  for (const auto& buffer : *buffers) {
    user_audio_buffer b;
    auto name = buffer->get_as<std::string>("name");
    auto size = buffer->get_as<size_t>("size");
    auto channels = buffer->get_as<size_t>("channels");

    if (!name) {
      std::ostringstream ss;
      ss << "Pipeline: Invalid audio buffer in ";
      ss << output.name;
      return report(ss.str());
    }

    b.name = *name;
    if (size) {
      b.size = *size;
    }
    if (channels) {
      b.channels = *channels;
    }
    output.audio_buffers.push_back(b);
  }

  return true;
}

static bool read_parameters(const shared_ptr<table> input,
                            user_object_template& output) {
  auto parameters = input->get_table_array("parameters");
  if (!parameters) {
    return true;
  }

  for (const auto& parameter : *parameters) {
    user_parameter p;
    auto name = parameter->get_as<std::string>("name");
    if (!name) {
      std::ostringstream ss;
      ss << "pipeline: parameter missing name ";
      ss << output.name << " ";
      return report(ss.str());
    }
    p.name = *name;

    auto value = parameter->get_array_of<double>("value");
    if (value) {
      p.value = *value;
    }

    auto step = parameter->get_as<double>("step");
    if (step) {
      p.step = *step;
    } else {
      p.step = 0;
    }

    auto min = parameter->get_array_of<double>("min");
    if (min) {
      p.min = *min;
    }

    auto max = parameter->get_array_of<double>("max");
    if (max) {
      p.max = *max;
    }

    auto help = parameter->get_as<std::string>("help");
    if (help) {
      p.help = *help;
    }

    output.parameters.push_back(p);
  }

  return true;
}

static bool read_fbo(const shared_ptr<table> input,
                     user_object_template& output) {
  auto fbo = input->get_table("fbo");
  if (!fbo) {
    return true;
  }

  user_fbo f;
  f.stencil = false;
  auto stencil = fbo->get_as<bool>("stencil");
  if (!stencil) {
    f.stencil = *stencil;
  }

  auto attachments = fbo->get_table_array("attachments");
  if (!attachments) {
    return true;
  }

  for (const auto& attachment : *attachments) {
    user_fbo_attachment a;
    auto type = attachment->get_as<std::string>("type");
    auto components = attachment->get_as<size_t>("components");
    auto width = attachment->get_as<size_t>("width");
    auto height = attachment->get_as<size_t>("height");

    if (!type || !components) {
      std::ostringstream ss;
      ss << "Pipeline: FBO attachment missing type or components";
      return report(ss.str());
    }

    a.type = *type;
    a.components = *components;

    if (width) {
      a.width = *width;
    }

    if (height) {
      a.height = *height;
    }

    f.attachments.push_back(a);
  }

  output.frame_buffers.push_back(f);
  return true;
}

static bool read_object_templates(const shared_ptr<table> input,
                                  user_plugin& output) {
  auto objects = input->get_table_array("objects");
  if (!objects) {
    return true;
  }

  for (const auto& object : *objects) {
    user_object_template t;
    auto name = object->get_as<std::string>("name");
    if (!name) {
      std::ostringstream ss;
      ss << "Pipeline: Plugin info missing object name ";
      ss << output.infopath;
      return report(ss.str());
    }

    auto type = object->get_as<std::string>("type");
    if (!type) {
      std::ostringstream ss;
      ss << "Pipeline: Plugin object missing type ";
      ss << *name << " ";
      ss << output.infopath;
      return report(ss.str());
    }

    t.name = *name;
    t.type = *type;
    for (const auto& task : obj_tasks) {
      if (!task(object, t)) {
        return false;
      }
    }

    output.objects.push_back(t);
  }

  return true;
}

static bool read_plugin_info(const shared_ptr<table> input,
                             user_plugin& output) {
  auto name = input->get_qualified_as<std::string>("library.name");
  if (!name) {
    std::ostringstream ss;
    ss << "Pipeline: Plugin info missing required 'name' in ";
    ss << output.infopath;
    return report(ss.str());
  }

  output.name = *name;
  if (!read_object_templates(input, output)) {
    return false;
  }

  auto base = output.basepath;
  for (auto& object : output.objects) {
    for (auto& shader : object.shaders) {
      shader.path = base + "/" + shader.path;

      if (!file_exists(shader.path)) {
        std::ostringstream ss;
        ss << "Pipeline: Shader doesn't exist ";
        ss << shader.path;
        return report(ss.str());
      }

      std::ifstream f(shader.path);
      f.seekg(0, std::ios::end);
      shader.code.reserve(f.tellg());
      f.seekg(0, std::ios::beg);

      shader.code.assign(std::istreambuf_iterator<char>(f),
                         std::istreambuf_iterator<char>());
    }
  }

  return true;
}

static bool read_plugins(const shared_ptr<table> input, user_data& output) {
  auto plugins = input->get_array_of<std::string>("plugins");
  if (!plugins) {
    return true;
  }

  for (const auto& plugin : *plugins) {
    user_plugin p;
    p.basepath = plugin;
    p.infopath = plugin + "/hans.toml";

    if (!file_exists(p.infopath)) {
      std::ostringstream ss;
      ss << "Pipeline: Unable to find plugin manifest, " << plugin;
      return report(ss.str());
    }

    auto table = cpptoml::parse_file(p.infopath);
    if (!read_plugin_info(table, p)) {
      std::ostringstream os;
      os << "Failed to process " << p.name;
      return report(os.str());
    }
    output.plugins.push_back(p);
  }

  return true;
}

static bool read_arg_value(const std::shared_ptr<cpptoml::table> input,
                           user_arg& output) {
  auto number = input->get_as<double>(output.name);
  if (!number) {
    auto boolean = input->get_as<bool>(output.name);
    if (!boolean) {
      auto string = input->get_as<std::string>(output.name);
      if (!string) {
        std::ostringstream ss;
        ss << "Pipeline: Unknown argument type for ";
        ss << output.name;
        return report(ss.str());
      } else {
        output.type = hans::Argument::STRING;
        output.string = *string;
      }
    } else {
      output.type = hans::Argument::BOOLEAN;
      output.boolean = *boolean;
    }
  } else {
    output.type = hans::Argument::NUMBER;
    output.number = *number;
  }
  return true;
}

static bool read_args(const shared_ptr<table> input, user_object& output) {
  auto args = input->get_table_array("args");
  if (args) {
    for (const auto& arg : *args) {
      for (const auto& inner : *arg) {
        user_arg a;
        a.name = inner.first;
        if (!read_arg_value(arg, a)) {
          return false;
        }
        output.arguments.push_back(a);
      }
    }
  }

  return true;
}

static bool read_objects(const shared_ptr<table> input, user_program& output) {
  auto objects = input->get_table("objects");
  if (!objects) {
    return true;
  }

  for (const auto& pair : *objects) {
    auto info = objects->get_table(pair.first);
    auto type = info->get_as<std::string>("object");
    if (!type) {
      return report("Object has no type");
    }

    user_object obj;
    obj.name = pair.first;
    obj.type = *type;
    if (!read_args(info, obj)) {
      return false;
    }

    output.objects.push_back(obj);
  }

  return true;
}

static bool read_connections(const shared_ptr<table> input,
                             vector<user_connection>& output,
                             const string& type) {
  auto connections = input->get_table_array(type);
  if (!connections) {
    return true;
  }

  for (const auto& conn : *connections) {
    user_connection c;
    c.source = *conn->get_as<std::string>("source");
    c.outlet = *conn->get_as<size_t>("outlet");
    c.target = *conn->get_as<std::string>("sink");
    c.inlet = *conn->get_as<size_t>("inlet");
    output.push_back(c);
  }

  return true;
}

static bool read_audio(const shared_ptr<table> input, user_program& output) {
  return read_connections(input, output.audio, "audio");
}

static bool read_graphics(const shared_ptr<table> input, user_program& output) {
  return read_connections(input, output.graphics, "graphics");
}

static bool read_modulator_port(const shared_ptr<table> input,
                                user_modulation_port& output) {
  auto object = input->get_as<std::string>("object");
  auto parameter = input->get_as<std::string>("parameter");
  auto component = input->get_as<size_t>("component");

  if (!object || !parameter || !component) {
    return report("Invalid modulation port");
  }

  output.object = *object;
  output.parameter = *parameter;
  output.component = *component;
  return true;
}

static bool read_modulators(const shared_ptr<table> input,
                            user_program& output) {
  auto modulators = input->get_table_array("modulators");
  if (!modulators) {
    return true;
  }

  for (const auto& modulator : *modulators) {
    auto source = modulator->get_table("source");
    auto target = modulator->get_table("target");
    if (!source || !target) {
      return report("Modulator most have source and targets");
    }

    user_modulator m;
    m.offset = 0;
    m.scale = 0;

    if (!read_modulator_port(source, m.source)) {
      return false;
    }
    if (!read_modulator_port(target, m.target)) {
      return false;
    }

    auto offset = modulator->get_as<double>("offset");
    if (offset) {
      m.offset = *offset;
    }

    auto scale = modulator->get_as<double>("scale");
    if (scale) {
      m.scale = *scale;
    }

    output.modulators.push_back(m);
  }

  return true;
}

static bool read_tracks(const shared_ptr<table> input, user_program& output) {
  auto sequences = input->get_table_array("tracks");
  if (!sequences) {
    return true;
  }

  for (const auto track : *sequences) {
    auto sequence = track->get_as<std::string>("sequence");
    if (!sequence) {
      return report("Track missing sequence");
    }

    user_track t;
    t.sequence = *sequence;

    auto target = track->get_table("target");
    if (!read_modulator_port(target, t.target)) {
      return false;
    }

    output.tracks.push_back(t);
  }

  return true;
}

static bool read_programs(const shared_ptr<table> input, user_data& output) {
  auto programs = input->get_table_array("programs");
  if (!programs) {
    return true;
  }

  for (const auto program : *programs) {
    auto name = program->get_as<std::string>("name");
    if (!name) {
      return report("Program name is required");
    }

    user_program pgm;
    pgm.name = *name;

    for (const auto& task : pgm_tasks) {
      if (!task(program, pgm)) {
        return false;
      }
    }

    output.programs.push_back(pgm);
  }

  return true;
}

static bool read_watchers(const shared_ptr<table> input, user_data& output) {
  auto reload = input->get_table("reload");
  if (!reload) {
    return true;
  }

  output.reload.delay = 0;

  auto paths = reload->get_array_of<std::string>("paths");
  if (paths) {
    output.reload.paths = *paths;
  }

  auto extensions = reload->get_array_of<std::string>("extensions");
  if (extensions) {
    output.reload.extensions = *extensions;
  }

  auto exclude = reload->get_array_of<std::string>("exclude");
  if (exclude) {
    output.reload.exclude = *exclude;
  }

  auto delay = input->get_as<size_t>("delay");
  if (delay) {
    output.reload.delay = *delay;
  }

  return true;
}

bool hans::load_config(const char* filepath, user_data& output) {
  if (!file_exists(filepath)) {
    std::ostringstream os;
    os << "File does not exist '" << filepath << "'";
    return report(os.str());
  }

  auto table = cpptoml::parse_file(filepath);

  for (const auto& task : top_tasks) {
    if (!task(table, output)) {
      return false;
    }
  }

  for (auto& program : output.programs) {
    for (auto& object : program.objects) {
      object.program = program.name;
      output.objects.push_back(object);
    }
  }

  return true;
}
