#include "hans/user_config_compiler.hpp"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include "hans/config.hpp"
#include "hans/configurator.hpp"
#include "hans/hasher.hpp"
#include "hans/interpreter.hpp"
#include "hans/object.hpp"
#include "hans/plugins.hpp"
#include "hans/primitives.hpp"
#include "hans/user_config.hpp"

using namespace hans;

static bool file_exists(const std::string filepath) {
  std::ifstream fs(filepath);
  return fs.good();
}

static bool report(const std::string& err) {
  std::cerr << "[HANS] Pipeline: " << err << std::endl;
  return false;
}

class string_buffer {
 public:
  hash add(const std::string& str) {
    auto hash = hans::hasher(str.c_str());
    auto& seen = _primitive.hashes;
    auto it = std::find(seen.begin(), seen.end(), hash);

    if (it == _primitive.hashes.end()) {
      _buffer << str;
      _primitive.hashes.push_back(hash);
      _primitive.lengths.push_back(str.size());
    }

    return hash;
  }

  hans::Strings primitive() {
    _primitive.buffer = _buffer.str();
    return _primitive;
  }

 private:
  std::ostringstream _buffer;
  hans::Strings _primitive;
};

struct resource_info {
  IConfigurator::Resources type;
  size_t value;
};

class pipeline_context {
 public:
  string_buffer strings;

  pipeline_context(const user_data& input) {
    for (const auto& plugin : input.plugins) {
      for (const auto& object : plugin.objects) {
        _template_map[object.name] = object;
        _template_hash_map[hans::hasher(object.name.c_str())] = object;
      }
    }
  }

  void set_resource_info(ObjectDef::ID id, resource_info info) {
    _resource_map[id].push_back(info);
  }

  const std::vector<resource_info>& resources(ObjectDef::ID id) {
    return _resource_map[id];
  }

  const user_object_template& get_object_tpl(const std::string& type) {
    return _template_map[type];
  }

  const user_object_template& get_object_tpl(const hash type) {
    return _template_hash_map[type];
  }

 private:
  std::unordered_map<hash, user_object_template> _template_hash_map;
  std::unordered_map<std::string, user_object_template> _template_map;
  std::unordered_map<ObjectDef::ID, std::vector<resource_info>> _resource_map;
};

class Configurator : public hans::IConfigurator {
 public:
  Configurator(ObjectDef::ID id, const std::vector<user_arg>& args,
               pipeline_context& ctx)
      : _id(id), _args(args), _ctx(ctx), _failure(false) {
  }

  bool good() {
    return !_failure;
  }

  virtual std::vector<Argument> arguments() override {
    std::vector<Argument> args;
    args.reserve(_args.size());
    for (const auto& arg : _args) {
      Argument a;
      a.name = _ctx.strings.add(arg.name);
      a.type = arg.type;
      switch (a.type) {
      case Argument::Types::NUMBER:
        a.number = arg.number;
        break;
      case Argument::Types::BOOLEAN:
        a.boolean = arg.boolean;
        break;
      case Argument::Types::STRING:
        a.string = _ctx.strings.add(arg.string);
        break;
      }
      args.push_back(a);
    }
    return args;
  }

  virtual void request(Configurator::Resources type, size_t value) override {
    resource_info info;
    info.type = type;
    info.value = value;
    _ctx.set_resource_info(_id, info);
  }

  virtual void missing(const char* name) override {
    std::ostringstream os;
    os << "Missing required argument '" << name << "'";
    report(os.str());
    _failure = true;
  }

  virtual void invalid(const char* name) override {
    std::ostringstream os;
    os << "Invalid argument for '" << name << "'";
    report(os.str());
    _failure = true;
  }

 private:
  const ObjectDef::ID _id;
  const std::vector<user_arg>& _args;
  pipeline_context& _ctx;
  bool _failure;
};

using task_fn =
    std::function<bool(const user_data&, EngineData&, pipeline_context&)>;

static bool settings_task(const user_data& input, EngineData& output,
                          pipeline_context& ctx) {
  output.settings = input.settings;
  return true;
}

static bool resolve_plugins_task(const user_data& input, EngineData& output,
                                 pipeline_context& ctx) {
  std::vector<std::string> search_paths = {HANS_LIBRARY_OUTPUT_DIRECTORY,
                                           HANS_LIBRARY_INSTALL_PATH};

  auto plugins = input.plugins;
  for (const auto& plugin : plugins) {
    auto found = false;
    for (const auto& sp : search_paths) {
      auto filepath = sp + "/" + plugin.name + HANS_SHARED_LIBRARY_SUFFIX;
      if (file_exists(filepath)) {
        output.plugins.filepaths.push_back(ctx.strings.add(filepath));
        found = true;
        break;
      }
    }

    if (!found) {
      std::ostringstream os;
      os << "Unable to resolve '" << plugin.name << "'";
      return report(os.str());
    }
  }

  return true;
}

// Sorting

static int index_of(const std::vector<user_object>& haystack,
                    const std::string& needle) {
  auto i = 0;
  for (const auto& candidate : haystack) {
    if (needle.compare(candidate.name) == 0) {
      return i;
    }
    i++;
  }
  return -1;
}

static std::vector<size_t> topological_sort(
    const std::vector<user_object>& objects,
    const std::vector<user_connection>& connections) {
  std::vector<size_t> sorted;
  std::vector<size_t> unorderable;
  std::unordered_map<size_t, size_t> dependencies;
  std::unordered_map<size_t, std::vector<size_t>> parents;

  for (const auto& conn : connections) {
    auto target = index_of(objects, conn.source);
    auto subtarget = index_of(objects, conn.target);
    if (target == -1 || subtarget == -1) {
      throw std::runtime_error("Pipeline: Object not found");
    }

    dependencies.emplace(subtarget, 0);
    parents[subtarget].push_back(target);
    ++dependencies[target];
  }

  for (const auto& pair : dependencies) {
    if (pair.second == 0) {
      sorted.push_back(pair.first);
    }
  }

  for (auto i = 0; i < sorted.size(); ++i) {
    auto target = sorted[i];
    for (auto& parent : parents[target])
      if (--dependencies[parent] == 0) {
        sorted.push_back(parent);
      }
  }

  for (const auto& pair : dependencies) {
    if (pair.second != 0) {
      unorderable.push_back(pair.first);
    }
  }

  std::reverse(sorted.begin(), sorted.end());
  return sorted;
}

static bool programs_task(const user_data& input, EngineData& output,
                          pipeline_context& ctx) {
  auto& programs = input.programs;
  auto& snd = output.programs.audio;
  auto& gfx = output.programs.graphics;
  auto ids = 0;

  for (const auto& program : programs) {
    output.programs.names.push_back(ctx.strings.add(program.name));

    auto snd_order = topological_sort(program.objects, program.audio);
    auto gfx_order = topological_sort(program.objects, program.graphics);

    auto snd_start = snd.objects.size();
    auto gfx_start = gfx.objects.size();

    for (const auto i : snd_order) {
      auto& object = program.objects[i];
      ObjectDef o;
      o.name = ctx.strings.add(object.type);
      o.id = ids;
      o.variable = ctx.strings.add(object.name);
      o.program = ctx.strings.add(program.name);

      snd.objects.push_back(o);
      snd.indices.push_back(snd.objects.size() - 1);
      ids++;
    }

    for (const auto i : gfx_order) {
      auto& object = program.objects[i];
      ObjectDef o;
      o.name = ctx.strings.add(object.type);
      o.id = ids;
      o.variable = ctx.strings.add(object.name);
      o.program = ctx.strings.add(program.name);

      gfx.objects.push_back(o);
      gfx.indices.push_back(gfx.objects.size() - 1);
      ids++;
    }

    snd.ranges.push_back({snd_start, snd.objects.size()});
    gfx.ranges.push_back({gfx_start, gfx.objects.size()});
  }

  return true;
}

// Configure

static user_object find_object(const user_data& input, hash variable,
                               hash program) {
  auto& objects = input.objects;
  for (const auto& object : objects) {
    auto _variable = hans::hasher(object.name.c_str());
    auto _program = hans::hasher(object.program.c_str());
    if (_variable == variable && _program == program) {
      return object;
    }
  }
  throw std::runtime_error("Pipeline: Unable to find object");
}

static bool configure_graph(hans::PluginManager& plugins, hans::Graphs& graphs,
                            const user_data& input, EngineData& output,
                            pipeline_context& ctx) {
  for (const auto& object : graphs.objects) {
    auto user = find_object(input, object.variable, object.program);
    Configurator configurator(object.id, user.arguments, ctx);

    auto instance = plugins.construct(object.name);
    instance->create(configurator);
    graphs.states.push_back(plugins.serialize(object.name, instance));
    plugins.destruct(object.name, instance);

    if (!configurator.good()) {
      std::ostringstream os;
      os << "Failed to configure '" << user.name << "' '" << user.type
         << "' in program '" << user.program << "'";
      return report(os.str());
    }
  }

  return true;
}

static bool configure_task(const user_data& input, EngineData& output,
                           pipeline_context& ctx) {
  hans::StringManager strings(ctx.strings.primitive());
  hans::PluginManager plugins(strings, output.plugins);
  configure_graph(plugins, output.programs.audio, input, output, ctx);
  configure_graph(plugins, output.programs.graphics, input, output, ctx);
  return true;
}

// Registers

static bool registers_allocate(const std::vector<user_connection>& connections,
                               const std::vector<ObjectDef>& objects,
                               const hans::ObjectDef::Types type,
                               EngineData& output) {
  auto bin = 0;
  for (const auto& connection : connections) {
    auto source = hans::hasher(connection.source.c_str());
    auto sink = hans::hasher(connection.target.c_str());
    auto found = 0;

    for (const auto& object : objects) {
      if (object.variable == source) {
        Register reg;
        reg.object = object.id;
        reg.type = type;
        reg.index = connection.outlet;
        reg.bin = bin;
        reg.readonly = false;
        output.registers.push_back(reg);
        found++;
      } else if (object.variable == sink) {
        Register reg;
        reg.object = object.id;
        reg.type = type;
        reg.index = connection.inlet;
        reg.bin = bin;
        reg.readonly = true;
        output.registers.push_back(reg);
        found++;
      }

      if (found == 2) {
        break;
      }
    }

    bin++;
  }
}

static bool registers_task(const user_data& input, EngineData& output,
                           pipeline_context& ctx) {
  for (const auto& program : input.programs) {
    registers_allocate(program.audio, output.programs.audio.objects,
                       ObjectDef::AUDIO, output);
    registers_allocate(program.graphics, output.programs.graphics.objects,
                       ObjectDef::GRAPHICS, output);
  }
  return true;
}

// Shaders

static bool shaders_task(const user_data& input, EngineData& output,
                         pipeline_context& ctx) {
  auto vertex = "vertex";
  auto fragment = "fragment";
  std::vector<hash> seen;

  for (const auto& object : input.objects) {
    auto type = hans::hasher(object.type.c_str());
    if (std::find(seen.begin(), seen.end(), type) != seen.end()) {
      continue;
    }

    seen.push_back(type);
    auto& tpl = ctx.get_object_tpl(object.type);

    for (const auto& shader : tpl.shaders) {
      graphics::Shader s;
      s.name = ctx.strings.add(shader.name);
      s.code = ctx.strings.add(shader.code);

      if (shader.type.compare(vertex) == 0) {
        s.type = graphics::Shader::VERTEX;
      } else if (shader.type.compare(fragment) == 0) {
        s.type = graphics::Shader::FRAGMENT;
      } else {
        std::ostringstream os;
        os << "Invalid shader type for " << shader.name << " in ";
        os << object.type;
        return report(os.str());
      }

      output.shaders.push_back(s);
    }
  }

  return true;
}

// Frame buffers

static bool frame_buffers_task(const user_data& input, EngineData& output,
                               pipeline_context& ctx) {
  auto color = "color";
  auto depth = "depth";
  auto stencil = "stencil";

  for (const auto& object : output.programs.graphics.objects) {
    auto& tpl = ctx.get_object_tpl(object.name);

    if (tpl.frame_buffers.size() > 1) {
      std::ostringstream os;
      os << "More than 1 frame buffer for " << object.id;
      return report(os.str());
    }

    for (const auto& fbo : tpl.frame_buffers) {
      graphics::FBO f;
      f.object = object.id;
      f.stencil_buffer = fbo.stencil;
      f.start = output.fbos_attachments.size();
      f.end = f.start + fbo.attachments.size();

      for (const auto& att : fbo.attachments) {
        graphics::FBO::Attachment a;
        a.width = (att.width == 0) ? input.settings.width : att.width;
        a.height = (att.height == 0) ? input.settings.height : att.height;
        a.components = att.components;

        if (att.type.compare(color) == 0) {
          a.type = graphics::FBO::Attachment::COLOR;
        } else if (att.type.compare(depth) == 0) {
          a.type = graphics::FBO::Attachment::DEPTH;
        } else if (att.type.compare(stencil) == 0) {
          a.type = graphics::FBO::Attachment::STENCIL;
        } else {
          std::ostringstream os;
          os << "Unknown FBO attachment type in " << tpl.name;
          return report(os.str());
        }

        output.fbos_attachments.push_back(a);
      }

      output.fbos.push_back(f);
    }
  }

  return true;
}

// Audio buffers

static bool audio_buffers_task(const user_data& input, EngineData& output,
                               pipeline_context& ctx) {
  auto offset = 0;
  auto& settings = output.settings;

  for (const auto& object : output.programs.audio.objects) {
    auto& tpl = ctx.get_object_tpl(object.name);
    for (const auto& buff : tpl.audio_buffers) {
      audio::Buffer b;
      b.name = ctx.strings.add(buff.name);
      b.channels = (buff.channels == 0) ? settings.channels : buff.channels;
      b.size = (buff.size == 0) ? settings.blocksize : buff.size;
      b.object = object.id;
      b.offset = offset;
      output.audio_buffers.push_back(b);
      offset += b.size * b.channels;
    }
  }

  return true;
}

// Parameters

static size_t parameters_process(Graphs& graph, EngineData& output,
                                 pipeline_context& ctx, size_t offset) {
  for (const auto& object : graph.objects) {
    auto& tpl = ctx.get_object_tpl(object.name);
    for (const auto& parameter : tpl.parameters) {
      auto size = parameter.value.size();
      if (size == 0) {
        size = 1;
      }

      Parameter p;
      p.object = object.id;
      p.name = ctx.strings.add(parameter.name);
      p.size = size;
      p.offset = offset;

      offset += p.size;
      output.parameters.handles.push_back(p);

      if (parameter.value.size() == 0) {
        output.parameters.buffer.push_back(0);
      } else {
        for (const auto& value : parameter.value) {
          output.parameters.buffer.push_back(value);
        }
      }
    }
  }
  return offset;
}

static bool parameters_task(const user_data& input, EngineData& output,
                            pipeline_context& ctx) {
  auto offset = parameters_process(output.programs.audio, output, ctx, 0);
  output.parameters.split = offset;
  parameters_process(output.programs.graphics, output, ctx, offset);
  return true;
}

// Modualtion

static int parameter_offset(const ObjectDef::ID id, const hash parameter,
                            const size_t component, EngineData& output) {
  for (const auto& handle : output.parameters.handles) {
    if (handle.object == id && handle.name == parameter) {
      return handle.offset + component;
    }
  }
  return -1;
}

static ObjectDef::ID find_object(const EngineData& output, const hash program,
                                 const hash variable) {
  for (const auto& object : output.programs.audio.objects) {
    if (object.program == program && object.variable == variable) {
      return object.id;
    }
  }
  for (const auto& object : output.programs.graphics.objects) {
    if (object.program == program && object.variable == variable) {
      return object.id;
    }
  }
  return -1;
}

enum ModulationType {
  AUDIO_AUDIO,
  AUDIO_GRAPHICS,
  GRAPHICS_GRAPHICS,
  GRAPHICS_AUDIO,
};

static ModulationType modulation_type(EngineData& output, const hash source,
                                      const hash target) {
  for (const auto& object : output.programs.audio.objects) {
    if (object.variable == source) {
      for (const auto& object : output.programs.audio.objects) {
        if (object.variable == target) {
          return AUDIO_AUDIO;
        }
      }
      return AUDIO_GRAPHICS;
    }
  }

  for (const auto& object : output.programs.graphics.objects) {
    if (object.variable == source) {
      for (const auto& object : output.programs.graphics.objects) {
        if (object.variable == target) {
          return GRAPHICS_GRAPHICS;
        }
      }
      return GRAPHICS_AUDIO;
    }
  }

  throw std::runtime_error("Unexpected modulation settings");
}

static bool modulators_task(const user_data& input, EngineData& output,
                            pipeline_context& ctx) {
  for (const auto& program : input.programs) {
    auto pgm = hans::hasher(program.name.c_str());
    for (const auto& modulator : program.modulators) {
      auto src_name = hans::hasher(modulator.source.object.c_str());
      auto dst_name = hans::hasher(modulator.target.object.c_str());

      auto src_param = hans::hasher(modulator.source.parameter.c_str());
      auto dst_param = hans::hasher(modulator.target.parameter.c_str());

      auto src_id = find_object(output, pgm, src_name);
      auto dst_id = find_object(output, pgm, dst_name);

      if (src_id == -1) {
        std::ostringstream os;
        os << "Invalid modulation source in " << program.name << " ";
        os << modulator.source.object;
        return report(os.str());
      }

      if (dst_id == -1) {
        std::ostringstream os;
        os << "Invalid modulation destination in " << program.name << " ";
        os << modulator.target.object;
        return report(os.str());
      }

      auto s_comp = modulator.source.component;
      auto d_comp = modulator.target.component;

      auto src_offset = parameter_offset(src_id, src_param, s_comp, output);
      auto dst_offset = parameter_offset(dst_id, dst_param, d_comp, output);

      if (src_offset == -1) {
        std::ostringstream os;
        os << "Failed to find modulation source in " << program.name << " ";
        os << modulator.source.object;
        return report(os.str());
      }

      if (src_offset == -1) {
        std::ostringstream os;
        os << "Failed to find modulation destination in ";
        os << program.name << " " << modulator.target.object;
        return report(os.str());
      }

      Modulator m;
      m.offset = modulator.offset;
      m.scale = modulator.scale;
      m.source = src_offset;
      m.destination = dst_offset;

      switch (modulation_type(output, src_name, dst_name)) {
      case GRAPHICS_GRAPHICS:
        output.modulators.graphics.local.push_back(m);
        break;
      case GRAPHICS_AUDIO:
        output.modulators.graphics.cross.push_back(m);
        break;
      case AUDIO_AUDIO:
        output.modulators.audio.local.push_back(m);
        break;
      case AUDIO_GRAPHICS:
        output.modulators.audio.cross.push_back(m);
        break;
      }
    }
  }
  return true;
}

static bool ring_buffers_task(const user_data& input, EngineData& output,
                              pipeline_context& ctx) {
  auto index = 0;
  for (const auto& object : output.programs.audio.objects) {
    auto& resources = ctx.resources(object.id);
    for (const auto& resource : resources) {
      if (resource.type == Configurator::RING_BUFFER) {
        RingBuffer rb;
        rb.name = resource.value;
        rb.index = index;
        rb.producer = object.id;
        index++;
        output.ring_buffers.push_back(rb);
      }
    }
  }
  return true;
}

static bool tracks_task(const user_data& input, EngineData& output,
                        pipeline_context& ctx) {
  for (const auto& program : input.programs) {
    auto pgm = ctx.strings.add(program.name);
    for (const auto& track : program.tracks) {
      auto obj = ctx.strings.add(track.target.object);
      std::stringstream ss(track.sequence);

      Track t;
      t.instructions = interpreter::compile(ss);
      t.parameter = ctx.strings.add(track.target.parameter);
      t.component = track.target.component;
      t.scale = track.scale;

      bool found = false;
      for (const auto& o : output.programs.graphics.objects) {
        if (o.program == pgm && o.variable == obj) {
          t.object = o.id;
          found = true;
        }
      }

      if (!found) {
        for (const auto& o : output.programs.audio.objects) {
          if (o.program == pgm && o.variable == obj) {
            t.object = o.id;
            found = true;
          }
        }
        if (!found) {
          std::ostringstream os;
          os << "Track, failed to find object '";
          os << track.target.object << "' in program '" << program.name << "'";
          return report(os.str());
        }
      }

      output.tracks.push_back(t);
    }
  }

  return true;
}

// clang-format off
static const task_fn tasks[] = {
  settings_task,
  resolve_plugins_task,
  programs_task,
  configure_task,
  registers_task,
  parameters_task,
  shaders_task,
  frame_buffers_task,
  audio_buffers_task,
  ring_buffers_task,
  modulators_task,
  tracks_task,
};
// clang-format on

bool hans::compile_config(const user_data& input, EngineData& output) {
  pipeline_context ctx(input);

  for (const auto& task : tasks) {
    if (!task(input, output, ctx)) {
      report("Failed to build engine data");
      return false;
    }
  }

  output.strings = ctx.strings.primitive();
  return true;
}
