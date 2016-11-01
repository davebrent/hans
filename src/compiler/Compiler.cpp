#include <libguile.h>
#include <cassert>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "hans/common/DataLoader.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/hasher.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/object.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ShaderManager.hpp"
#include "hans/engine/gl.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::engine::audio;
using namespace hans::engine::graphics;

static scm_t_bits data_writer_tag;

static uint64_t lst_length(SCM lst) {
  assert(scm_is_true(scm_list_p(lst)) == 1);
  return scm_to_int(scm_length(lst));
}

static bool str_eqaul_sym(const char* str, SCM sym) {
  assert(scm_is_true(scm_symbol_p(sym)) == 1);
  return scm_is_true(scm_eq_p(scm_from_locale_symbol(str), sym)) == 1;
}

static hash scm_to_hans_hash(SCM value) {
  assert(scm_is_true(scm_string_p(value)) == 1);
  auto string = scm_to_locale_string(value);
  auto hashed = hasher(string);
  std::free(string);
  return hashed;
}

static hash scm_to_hans_hash(StringManager& strings, SCM value) {
  auto str = scm_to_locale_string(value);
  auto hashed = strings.intern(str);
  std::free(str);
  return hashed;
}

static DataWriter* scm_to_writer(SCM writer) {
  scm_assert_smob_type(data_writer_tag, writer);
  return reinterpret_cast<DataWriter*>(SCM_SMOB_DATA(writer));
}

template <typename T>
static SCM write_list(SCM writer, DataFile::Types type, std::vector<T> lst) {
  auto size = sizeof(T) * lst.size();
  auto instance = scm_to_writer(writer);
  instance->stage(type, static_cast<void*>(&lst[0]), size);
  return scm_from_int(size);
}

static ObjectDef::Types scm_to_hans_obj_type(SCM type) {
  if (str_eqaul_sym("graphics", type)) {
    return ObjectDef::Types::GRAPHICS;
  } else if (str_eqaul_sym("audio", type)) {
    return ObjectDef::Types::AUDIO;
  }

  throw std::runtime_error("Unknown object type");
}

static Shader::Types scm_to_hans_shader_type(SCM type) {
  if (str_eqaul_sym("vertex", type)) {
    return Shader::Types::VERTEX;
  } else if (str_eqaul_sym("fragment", type)) {
    return Shader::Types::FRAGMENT;
  }

  throw std::runtime_error("Unknown shader type");
}

static SCM write_modulators(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Modulator> result;
  result.reserve(len);

  auto zero = scm_from_int(0);
  auto one = scm_from_int(1);
  auto two = scm_from_int(2);
  auto three = scm_from_int(3);
  auto four = scm_from_int(4);
  auto five = scm_from_int(5);
  auto six = scm_from_int(6);
  auto seven = scm_from_int(7);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));

    Modulator::Port source;
    source.object = scm_to_int(scm_list_ref(item, zero));
    source.parameter = scm_to_hans_hash(scm_list_ref(item, one));
    source.component = scm_to_int(scm_list_ref(item, two));

    Modulator::Port dest;
    dest.object = scm_to_int(scm_list_ref(item, three));
    dest.parameter = scm_to_hans_hash(scm_list_ref(item, four));
    dest.component = scm_to_int(scm_list_ref(item, five));

    Modulator modulator;
    modulator.source = source;
    modulator.dest = dest;
    modulator.offset = scm_to_double(scm_list_ref(item, six));
    modulator.scale = scm_to_double(scm_list_ref(item, seven));
    result.push_back(modulator);
  }

  return write_list<Modulator>(writer, DataFile::Types::MODULATORS, result);
}

static SCM write_libraries(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Library> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto path = scm_list_ref(lst, scm_from_int(i));

    Library library;
    library.filepath = scm_to_hans_hash(path);
    result.push_back(library);
  }

  return write_list<Library>(writer, DataFile::Types::LIBRARIES, result);
}

static SCM write_objects(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<ObjectDef> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, scm_from_locale_symbol("instance-id"));
    auto name = scm_assq_ref(alist, scm_from_locale_symbol("name"));
    auto type = scm_assq_ref(alist, scm_from_locale_symbol("type"));

    ObjectDef item;
    item.id = scm_to_int(id);
    item.type = scm_to_hans_obj_type(type);
    item.name = scm_to_hans_hash(name);
    item.size = 0;
    item.create = nullptr;
    item.destroy = nullptr;
    item.instance = nullptr;
    result.push_back(item);
  }

  return write_list<ObjectDef>(writer, DataFile::Types::OBJECTS, result);
}

static SCM write_object_data(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  auto size = 0;

  for (auto i = 0; i < len; ++i) {
    auto bv = scm_list_ref(lst, scm_from_int(i));
    assert(scm_is_true(scm_bytevector_p(bv)) == 1);
    size += SCM_BYTEVECTOR_LENGTH(bv);
  }

  LinearAllocator allocator(size);

  for (auto i = 0; i < len; ++i) {
    auto bv = scm_list_ref(lst, scm_from_int(i));
    auto size = SCM_BYTEVECTOR_LENGTH(bv);
    ;
    auto src = SCM_BYTEVECTOR_CONTENTS(bv);
    auto dest = allocator.allocate(size);
    std::memcpy(dest, src, size);
  }

  auto instance = scm_to_writer(writer);
  instance->stage(DataFile::Types::OBJECTS_DATA, allocator.start(), size);
  return scm_from_int(size);
}

static SCM write_parameters(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Parameter> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto instance = scm_assq_ref(alist, scm_from_locale_symbol("instance-id"));
    auto name = scm_assq_ref(alist, scm_from_locale_symbol("name"));
    auto size = scm_assq_ref(alist, scm_from_locale_symbol("size"));
    auto offset = scm_assq_ref(alist, scm_from_locale_symbol("value"));

    Parameter item;
    item.object = scm_to_int(instance);
    item.name = scm_to_hans_hash(name);
    item.size = scm_to_int(size);
    item.offset = scm_to_int(offset);
    result.push_back(item);
  }

  return write_list<Parameter>(writer, DataFile::Types::PARAMETERS, result);
}

static SCM write_parameter_values(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Parameter::Value> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));
    result.push_back(scm_to_double(item));
  }

  return write_list<Parameter::Value>(writer, DataFile::Types::PARAMETER_VALUES,
                                      result);
}

static SCM write_programs(SCM writer, SCM lst, SCM nodata) {
  auto len = lst_length(lst);
  std::vector<Program> result;
  result.reserve(len);

  auto nodata_value = scm_to_int(nodata);

  auto sym_name = scm_from_locale_symbol("name");
  auto sym_audio = scm_from_locale_symbol("audio");
  auto sym_graphics = scm_from_locale_symbol("graphics");

  auto zero = scm_from_int(0);
  auto one = scm_from_int(1);
  auto two = scm_from_int(2);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto name = scm_assq_ref(alist, sym_name);
    auto audio = scm_assq_ref(alist, sym_audio);
    auto graphics = scm_assq_ref(alist, sym_graphics);

    Program item;
    item.name = scm_to_hans_hash(name);

    item.audio.id = 0;
    item.audio.start = nodata_value;
    item.audio.end = nodata_value;

    item.graphics.id = 0;
    item.graphics.start = nodata_value;
    item.graphics.end = nodata_value;

    if (lst_length(audio) == 3) {
      item.audio.id = scm_to_int(scm_list_ref(audio, zero));
      item.audio.start = scm_to_int(scm_list_ref(audio, one));
      item.audio.end = scm_to_int(scm_list_ref(audio, two));
    }

    if (lst_length(graphics) == 3) {
      item.graphics.id = scm_to_int(scm_list_ref(graphics, zero));
      item.graphics.start = scm_to_int(scm_list_ref(graphics, one));
      item.graphics.end = scm_to_int(scm_list_ref(graphics, two));
    }

    result.push_back(item);
  }

  return write_list<Program>(writer, DataFile::Types::PROGRAMS, result);
}

static SCM write_graphs(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<size_t> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));
    result.push_back(scm_to_int(item));
  }

  return write_list<size_t>(writer, DataFile::Types::CHAINS, result);
}

static SCM write_registers(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Register> result;
  result.reserve(len);

  auto zero = scm_from_int(0);
  auto one = scm_from_int(1);
  auto two = scm_from_int(2);
  auto three = scm_from_int(3);
  auto four = scm_from_int(4);
  auto five = scm_from_int(5);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));
    Register reg;
    reg.object = scm_to_int(scm_list_ref(item, zero));
    reg.type = scm_to_hans_obj_type(scm_list_ref(item, one));
    reg.graph = scm_to_int(scm_list_ref(item, two));
    reg.index = scm_to_int(scm_list_ref(item, three));
    reg.bin = scm_to_int(scm_list_ref(item, four));
    reg.readonly = scm_to_bool(scm_list_ref(item, five));
    result.push_back(reg);
  }

  return write_list<Register>(writer, DataFile::Types::REGISTERS, result);
}

static SCM write_strings(SCM writer, SCM lst) {
  auto len = lst_length(lst);

  std::vector<size_t> lengths;
  lengths.reserve(len);

  std::vector<size_t> offsets;
  offsets.reserve(len);

  std::vector<hash> hashes;
  hashes.reserve(len);

  auto sum = 0;
  for (auto i = 0; i < len; ++i) {
    auto str = scm_list_ref(lst, scm_from_int(i));
    assert(scm_is_true(scm_string_p(str)) == 1);
    auto length = scm_to_int(scm_string_length(str)) + 1;

    hashes.push_back(scm_to_hans_hash(str));
    lengths.push_back(length);
    offsets.push_back(sum);
    sum += length;
  }

  LinearAllocator allocator(sum);

  for (auto i = 0; i < len; ++i) {
    auto str = scm_list_ref(lst, scm_from_int(i));
    auto src = scm_to_locale_string(str);
    auto size = lengths.at(i);
    auto dest = allocator.allocate(size);
    std::memcpy(dest, static_cast<void*>(src), size);
    std::free(src);
  }

  auto hashes_size = sizeof(hash) * len;
  auto offsets_size = sizeof(size_t) * len;

  auto instance = scm_to_writer(writer);
  instance->stage(DataFile::Types::STRING_HASHES,
                  static_cast<void*>(&hashes[0]), hashes_size);
  instance->stage(DataFile::Types::STRING_OFFSETS,
                  static_cast<void*>(&offsets[0]), offsets_size);
  instance->stage(DataFile::Types::STRINGS, allocator.start(), sum);
  return scm_from_int(sum + hashes_size + offsets_size);
}

static SCM write_shaders(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Shader> result;
  result.reserve(len);

  auto sym_type = scm_from_locale_symbol("type");
  auto sym_name = scm_from_locale_symbol("name");
  auto sym_code = scm_from_locale_symbol("code");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto type = scm_assq_ref(alist, sym_type);
    auto name = scm_assq_ref(alist, sym_name);
    auto code = scm_assq_ref(alist, sym_code);

    Shader item;
    item.type = scm_to_hans_shader_type(type);
    item.name = scm_to_hans_hash(name);
    item.code = scm_to_hans_hash(code);
    result.push_back(item);
  }

  return write_list<Shader>(writer, DataFile::Types::SHADERS, result);
}

static SCM write_fbos(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<FBO> result;
  result.reserve(len);

  auto sym_stencil_buffer = scm_from_locale_symbol("stencil-buffer");
  auto sym_attachments = scm_from_locale_symbol("attachments");
  auto sym_instance_id = scm_from_locale_symbol("instance-id");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, sym_instance_id);
    auto stencil = scm_assq_ref(alist, sym_stencil_buffer);
    auto attachments = scm_assq_ref(alist, sym_attachments);

    FBO item;
    item.object = scm_to_int(id);
    item.stencil_buffer = scm_to_bool(stencil);
    item.start = scm_to_int(scm_list_ref(attachments, scm_from_int(0)));
    item.end = scm_to_int(scm_list_ref(attachments, scm_from_int(1)));
    result.push_back(item);
  }

  return write_list<FBO>(writer, DataFile::Types::FBOS, result);
}

static FBO::Attachment::Types scm_to_fbo_attachment_type(SCM type) {
  if (str_eqaul_sym("color", type)) {
    return FBO::Attachment::Types::COLOR;
  } else if (str_eqaul_sym("depth", type)) {
    return FBO::Attachment::Types::DEPTH;
  } else if (str_eqaul_sym("stencil", type)) {
    return FBO::Attachment::Types::STENCIL;
  }

  throw std::runtime_error("Unknown fbo attachment type");
}

static SCM write_fbo_attachments(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<FBO::Attachment> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto type = scm_assq_ref(alist, scm_from_locale_symbol("type"));
    auto width = scm_assq_ref(alist, scm_from_locale_symbol("width"));
    auto height = scm_assq_ref(alist, scm_from_locale_symbol("height"));
    auto components = scm_assq_ref(alist, scm_from_locale_symbol("components"));

    FBO::Attachment item;
    item.type = scm_to_fbo_attachment_type(type);
    item.width = scm_to_int(width);
    item.height = scm_to_int(height);
    item.components = scm_to_int(components);
    result.push_back(item);
  }

  return write_list<FBO::Attachment>(writer, DataFile::Types::FBO_ATTACHMENTS,
                                     result);
}

static SCM write_audio_buffers(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<Buffer> result;
  result.reserve(len);

  auto sym_instance_id = scm_from_locale_symbol("instance-id");
  auto sym_name = scm_from_locale_symbol("name");
  auto sym_channels = scm_from_locale_symbol("channels");
  auto sym_size = scm_from_locale_symbol("size");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, sym_instance_id);
    auto name = scm_assq_ref(alist, sym_name);
    auto channels = scm_assq_ref(alist, sym_channels);
    auto size = scm_assq_ref(alist, sym_size);

    Buffer item;
    item.object = scm_to_int(id);
    item.name = scm_to_hans_hash(name);
    item.channels = scm_to_int(channels);
    item.size = scm_to_int(size);
    item.offset = 0;
    result.push_back(item);
  }

  return write_list<Buffer>(writer, DataFile::Types::AUDIO_BUFFERS, result);
}

static SCM write_ring_buffers(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<RingBuffer> result;
  result.reserve(len);

  auto sym_instance_id = scm_from_locale_symbol("instance-id");
  auto sym_name = scm_from_locale_symbol("name");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, sym_instance_id);
    auto name = scm_assq_ref(alist, sym_name);

    RingBuffer item;
    item.producer = scm_to_int(id);
    item.name = scm_to_hans_hash(name);
    item.offset = 0;
    item.index = 0;
    result.push_back(item);
  }

  return write_list<RingBuffer>(writer, DataFile::Types::RING_BUFFERS, result);
}

static SCM make_hans_file_writer(SCM size) {
  void* place = scm_gc_malloc(sizeof(DataWriter), "hans-file-writer");
  auto writer = new (place) DataWriter(scm_to_int(size));
  return scm_new_smob(data_writer_tag, (scm_t_bits)writer);
}

static SCM hans_file_write(SCM writer, SCM dest) {
  scm_assert_smob_type(data_writer_tag, writer);
  auto instance = reinterpret_cast<DataWriter*>(SCM_SMOB_DATA(writer));
  auto str = scm_to_locale_string(dest);
  instance->write(str);
  std::free(str);
  return SCM_BOOL_T;
}

static SCM hans_resource_type_to_scm(IPatcher::Resources t) {
  switch (t) {
  case IPatcher::Resources::PARAMETER:
    return scm_from_locale_symbol("parameter");
  case IPatcher::Resources::SHADER:
    return scm_from_locale_symbol("shader");
  case IPatcher::Resources::AUDIO_BUFFER:
    return scm_from_locale_symbol("audio-buffer");
  case IPatcher::Resources::FRAME_BUFFER:
    return scm_from_locale_symbol("fbo");
  case IPatcher::Resources::INLET:
    return scm_from_locale_symbol("inlet");
  case IPatcher::Resources::OUTLET:
    return scm_from_locale_symbol("outlet");
  case IPatcher::Resources::RING_BUFFER:
    return scm_from_locale_symbol("ring-buffer");
  }

  throw std::runtime_error("Unknown resource type");
}

static Argument::Types scm_to_hans_argument_type(SCM value) {
  if (scm_to_bool(scm_boolean_p(value)) == 1) {
    return Argument::Types::BOOLEAN;
  } else if (scm_to_bool(scm_number_p(value)) == 1) {
    return Argument::Types::NUMBER;
  } else if (scm_to_bool(scm_string_p(value)) == 1) {
    return Argument::Types::STRING;
  }

  throw std::runtime_error("Unknown argument type");
}

static SCM valid_shaders(SCM lst) {
  auto strings = StringManager(96768);
  auto len = lst_length(lst);
  auto shader_vec = std::vector<Shader>();
  shader_vec.reserve(len);

  auto sym_type = scm_from_locale_symbol("type");
  auto sym_name = scm_from_locale_symbol("name");
  auto sym_code = scm_from_locale_symbol("code");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto type = scm_assq_ref(alist, sym_type);
    auto name = scm_assq_ref(alist, sym_name);
    auto code = scm_assq_ref(alist, sym_code);

    Shader item;
    item.type = scm_to_hans_shader_type(type);
    item.name = scm_to_hans_hash(strings, name);
    item.code = scm_to_hans_hash(strings, code);
    shader_vec.push_back(item);
  }

  auto shader_list = ListView<Shader>(&shader_vec[0], len);

  bool res = glfwInit();
  if (res != GL_TRUE) {
    return SCM_BOOL_F;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
  glfwWindowHint(GLFW_FOCUSED, GL_FALSE);
  glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
  glfwWindowHint(GLFW_DECORATED, GL_FALSE);
  auto window = glfwCreateWindow(480, 320, "Shaders", nullptr, nullptr);
  glfwMakeContextCurrent(window);

  auto shader_manager = ShaderManager(strings, shader_list);
  auto out = SCM_EOL;

  for (const auto& shader : shader_list) {
    auto message = shader_manager.validate(shader.name);

    auto out_valid = SCM_BOOL_T;
    auto out_message = SCM_BOOL_F;

    if (message != nullptr) {
      out_valid = SCM_BOOL_F;
      out_message = scm_from_locale_string(message);
      delete[] message;
    }

    out = scm_cons(scm_cons(out_valid, out_message), out);
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return scm_reverse(out);
}

static std::vector<Library> scm_to_hans_libs(SCM libraries,
                                             StringManager& strings) {
  int len = lst_length(libraries);
  std::vector<Library> result;
  result.reserve(len);

  for (int i = 0; i < len; ++i) {
    Library lib;
    auto path = scm_to_locale_string(scm_list_ref(libraries, scm_from_int(i)));
    lib.filepath = strings.intern(path);
    std::free(path);
    result.push_back(lib);
  }

  return result;
}

/// Fill in result with num args
static void scm_to_hans_obj_args(SCM args, StringManager& strings,
                                 Argument* out, int len) {
  for (int i = 0; i < len; ++i) {
    auto arg = scm_list_ref(args, scm_from_int(i));
    auto key = scm_car(arg);
    auto value = scm_cdr(arg);

    char* str = scm_to_locale_string(scm_symbol_to_string(key));

    Argument hans_arg;
    hans_arg.name = strings.intern(str);
    hans_arg.type = scm_to_hans_argument_type(value);

    switch (hans_arg.type) {
    case Argument::Types::BOOLEAN:
      hans_arg.boolean = scm_to_bool(value) == 1;
      break;
    case Argument::Types::NUMBER:
      hans_arg.number = scm_to_double(value);
      break;
    case Argument::Types::STRING:
      hans_arg.string = strings.intern(scm_to_locale_string(value));
      break;
    }

    out[i] = hans_arg;
  }
}

static std::vector<ObjectDef> scm_to_hans_objs(SCM objects,
                                               StringManager& strings) {
  int len = lst_length(objects);
  std::vector<ObjectDef> result;
  result.reserve(len);

  auto key_name = scm_from_locale_symbol("name");

  for (int i = 0; i < len; ++i) {
    auto user_object = scm_list_ref(objects, scm_from_int(i));

    ObjectDef obj;
    obj.size = 0;
    obj.create = nullptr;

    auto name = scm_to_locale_string(scm_assq_ref(user_object, key_name));
    obj.name = strings.intern(name);
    std::free(name);

    result.push_back(obj);
  }

  return result;
}

class GuilePatcher : public IPatcher {
 public:
  GuilePatcher(const SCM& objects, StringManager& strings);
  ~GuilePatcher();
  virtual ListView<Argument> arguments();
  virtual void missing(Argument::Types type, hash name);
  virtual void request(IPatcher::Resources type, size_t value);

  std::vector<SCM> get_requests();
  void next();

 private:
  const SCM& m_objects;
  StringManager& m_strings;

  std::vector<SCM> m_requests;

  Argument* m_args;
  std::vector<int> m_arg_offsets;
  std::vector<int> m_arg_lengths;

  size_t m_current_object;
};

GuilePatcher::GuilePatcher(const SCM& objects, StringManager& strings)
    : m_objects(objects), m_strings(strings) {
  m_current_object = 0;

  auto total_num_args = 0;
  auto arg_key = scm_from_locale_symbol("args");
  for (int i = 0; i < lst_length(m_objects); ++i) {
    auto obj = scm_list_ref(m_objects, scm_from_int(i));
    auto args = scm_assq_ref(obj, arg_key);

    // FIXME: Check that its not #f
    auto len = lst_length(args);

    m_arg_offsets.push_back(total_num_args);
    m_arg_lengths.push_back(len);
    total_num_args += len;
  }

  m_args = new Argument[total_num_args];

  for (int i = 0; i < lst_length(objects); ++i) {
    auto num_args = m_arg_lengths.at(i);
    if (num_args == 0) {
      continue;
    }

    auto obj = scm_list_ref(objects, scm_from_int(i));
    auto args = scm_assq_ref(obj, arg_key);

    auto out = &m_args[m_arg_offsets.at(i)];
    scm_to_hans_obj_args(args, strings, out, num_args);
  }
}

GuilePatcher::~GuilePatcher() {
  delete[] m_args;
}

ListView<Argument> GuilePatcher::arguments() {
  auto offset = m_arg_offsets.at(m_current_object);
  auto length = m_arg_lengths.at(m_current_object);
  return ListView<Argument>(&m_args[offset], length);
}

void GuilePatcher::missing(Argument::Types type, hash name) {
  // TODO: Currently unused
}

void GuilePatcher::request(IPatcher::Resources type, size_t value) {
  SCM req;

  switch (type) {
  case IPatcher::Resources::INLET:
  case IPatcher::Resources::OUTLET: {
    req = scm_cons(hans_resource_type_to_scm(type), scm_from_int(value));
    break;
  }
  case IPatcher::Resources::RING_BUFFER: {
    auto name = m_strings.lookup(value);
    auto str = scm_from_locale_string(name);
    req = scm_cons(hans_resource_type_to_scm(type), str);
    break;
  }
  default:
    throw std::runtime_error("Unkown resource request");
  }

  m_requests.push_back(req);
}

std::vector<SCM> GuilePatcher::get_requests() {
  return m_requests;
}

void GuilePatcher::next() {
  m_requests.clear();
  m_current_object += 1;
}

/// Returns an alist for each object instance describing its runtime resources
static SCM get_object_info(SCM libraries, SCM objects) {
  StringManager strings(16384 /* 16kb */);

  std::vector<Library> libs = scm_to_hans_libs(libraries, strings);
  std::vector<ObjectDef> objs = scm_to_hans_objs(objects, strings);

  auto patcher = GuilePatcher(objects, strings);

  auto object_list = ListView<ObjectDef>(&objs[0], objs.size());
  auto library_list = ListView<Library>(&libs[0], libs.size());
  auto library_manager =
      engine::LibraryManager(strings, object_list, library_list);

  auto total_objects_bytes = 0;
  for (auto& obj : objs) {
    assert(obj.create != nullptr);
    assert(obj.size != 0);
    total_objects_bytes += obj.size;
  }

  LinearAllocator allocator(total_objects_bytes);

  SCM out = SCM_EOL;

  for (auto& obj : objs) {
    auto buff = static_cast<char*>(allocator.allocate(obj.size));

    Object* instance = static_cast<Object*>(obj.create(0, buff));
    instance->create(patcher);

    SCM lst = SCM_EOL;
    for (auto& req : patcher.get_requests()) {
      lst = scm_cons(req, lst);
    }

    auto bv = scm_c_make_bytevector(obj.size);
    auto dest = SCM_BYTEVECTOR_CONTENTS(bv);
    std::memcpy(dest, obj.serialize(instance), obj.size);

    patcher.next();
    obj.destroy(instance);

    out = scm_cons(scm_list_2(bv, lst), out);
  }

  objs.clear();
  return scm_reverse(out);
}

extern "C" {
void scm_init_hans_compiler_module() {
  scm_c_define_gsubr("make-hans-file-writer", 1, 0, 0,
                     (scm_t_subr)make_hans_file_writer);
  auto data_writer_size = sizeof(DataWriter);
  data_writer_tag = scm_make_smob_type("hans-file-writer", data_writer_size);
  scm_c_define_gsubr("hans-file-write", 2, 0, 0, (scm_t_subr)hans_file_write);

  scm_c_define_gsubr("write-modulators", 2, 0, 0, (scm_t_subr)write_modulators);
  scm_c_define_gsubr("write-libraries", 2, 0, 0, (scm_t_subr)write_libraries);
  scm_c_define_gsubr("write-objects", 2, 0, 0, (scm_t_subr)write_objects);
  scm_c_define_gsubr("write-object-data", 2, 0, 0,
                     (scm_t_subr)write_object_data);
  scm_c_define_gsubr("write-programs", 3, 0, 0, (scm_t_subr)write_programs);
  scm_c_define_gsubr("write-graphs", 2, 0, 0, (scm_t_subr)write_graphs);
  scm_c_define_gsubr("write-parameters", 2, 0, 0, (scm_t_subr)write_parameters);
  scm_c_define_gsubr("write-parameter-values", 2, 0, 0,
                     (scm_t_subr)write_parameter_values);
  scm_c_define_gsubr("write-registers", 2, 0, 0, (scm_t_subr)write_registers);
  scm_c_define_gsubr("write-shaders", 2, 0, 0, (scm_t_subr)write_shaders);
  scm_c_define_gsubr("write-fbos", 2, 0, 0, (scm_t_subr)write_fbos);
  scm_c_define_gsubr("write-fbo-attachments", 2, 0, 0,
                     (scm_t_subr)write_fbo_attachments);
  scm_c_define_gsubr("write-audio-buffers", 2, 0, 0,
                     (scm_t_subr)write_audio_buffers);
  scm_c_define_gsubr("write-ring-buffers", 2, 0, 0,
                     (scm_t_subr)write_ring_buffers);
  scm_c_define_gsubr("write-strings", 2, 0, 0, (scm_t_subr)write_strings);

  scm_c_define_gsubr("valid-shaders?", 1, 0, 0, (scm_t_subr)valid_shaders);
  scm_c_define_gsubr("get-object-info", 2, 0, 0, (scm_t_subr)get_object_info);
}
}
