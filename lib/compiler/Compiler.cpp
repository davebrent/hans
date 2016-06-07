#include <libguile.h>
#include <cassert>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "hans/common/DataLoader.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/hasher.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/LibraryManager.hpp"

using namespace hans;

static scm_t_bits data_writer_tag;

static uint64_t lst_length(SCM lst) {
  assert(scm_is_true(scm_list_p(lst)) == 1);
  return scm_to_int(scm_length(lst));
}

static bool str_eqaul_sym(const char* str, SCM sym) {
  assert(scm_is_true(scm_symbol_p(sym)) == 1);
  return scm_is_true(scm_eq_p(scm_from_locale_symbol(str), sym)) == 1;
}

static hans_hash scm_to_hans_hash(SCM value) {
  assert(scm_is_true(scm_string_p(value)) == 1);
  auto string = scm_to_locale_string(value);
  auto hash = common::hasher(string);
  std::free(string);
  return hash;
}

static common::DataWriter* scm_to_writer(SCM writer) {
  scm_assert_smob_type(data_writer_tag, writer);
  return reinterpret_cast<common::DataWriter*>(SCM_SMOB_DATA(writer));
}

template <typename T>
static SCM write_list(SCM writer, hans_blob_type type, std::vector<T> lst) {
  auto size = sizeof(T) * lst.size();
  auto instance = scm_to_writer(writer);
  instance->stage(type, static_cast<void*>(&lst[0]), size);
  return scm_from_int(size);
}

static hans_object_type scm_to_hans_obj_type(SCM type) {
  if (str_eqaul_sym("graphics", type)) {
    return HANS_GRAPHICS;
  } else if (str_eqaul_sym("audio", type)) {
    return HANS_AUDIO;
  }

  throw std::runtime_error("Unknown object type");
}

static hans_shader_type scm_to_hans_shader_type(SCM type) {
  if (str_eqaul_sym("vertex", type)) {
    return HANS_SHADER_VERTEX;
  } else if (str_eqaul_sym("fragment", type)) {
    return HANS_SHADER_FRAGMENT;
  }

  throw std::runtime_error("Unknown shader type");
}

static hans_resource_type scm_to_hans_resource_type(SCM type) {
  if (str_eqaul_sym("parameter", type)) {
    return HANS_PARAMETER;
  } else if (str_eqaul_sym("shader", type)) {
    return HANS_SHADER;
  } else if (str_eqaul_sym("audio-buffer", type)) {
    return HANS_AUDIO_BUFFER;
  } else if (str_eqaul_sym("fbo", type)) {
    return HANS_FRAME_BUFFER;
  } else if (str_eqaul_sym("inlet", type)) {
    return HANS_INLET;
  } else if (str_eqaul_sym("outlet", type)) {
    return HANS_OUTLET;
  }

  throw std::runtime_error("Unknown resource type");
}

static SCM write_libraries(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_library> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto path = scm_list_ref(lst, scm_from_int(i));

    hans_library library;
    library.name = 0;
    library.handle = nullptr;
    library.filepath = scm_to_hans_hash(path);
    result.push_back(library);
  }

  return write_list<hans_library>(writer, HANS_BLOB_LIBRARIES, result);
}

static SCM write_objects(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_object> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, scm_from_locale_symbol("instance-id"));
    auto name = scm_assq_ref(alist, scm_from_locale_symbol("name"));
    auto type = scm_assq_ref(alist, scm_from_locale_symbol("type"));
    // auto size = scm_assq_ref(alist, scm_from_locale_symbol("size"));

    hans_object item;
    item.id = scm_to_int(id);
    item.type = scm_to_hans_obj_type(type);
    item.name = scm_to_hans_hash(name);
    item.size = 0;
    item.make = nullptr;
    item.destroy = nullptr;
    result.push_back(item);
  }

  return write_list<hans_object>(writer, HANS_BLOB_OBJECTS, result);
}

static SCM write_object_data(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  auto size = 0;

  for (auto i = 0; i < len; ++i) {
    auto bv = scm_list_ref(lst, scm_from_int(i));
    assert(scm_is_true(scm_bytevector_p(bv)) == 1);
    size += SCM_BYTEVECTOR_LENGTH(bv);
  }

  common::LinearAllocator allocator(size);

  for (auto i = 0; i < len; ++i) {
    auto bv = scm_list_ref(lst, scm_from_int(i));
    auto size = SCM_BYTEVECTOR_LENGTH(bv);
    ;
    auto src = SCM_BYTEVECTOR_CONTENTS(bv);
    auto dest = allocator.allocate(size);
    std::memcpy(dest, src, size);
  }

  auto instance = scm_to_writer(writer);
  instance->stage(HANS_BLOB_OBJECTS_DATA, allocator.start(), size);
  return scm_from_int(size);
}

static SCM write_parameters(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_parameter> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, scm_from_locale_symbol("instance-id"));
    auto name = scm_assq_ref(alist, scm_from_locale_symbol("name"));
    auto size = scm_assq_ref(alist, scm_from_locale_symbol("size"));
    auto offset = scm_assq_ref(alist, scm_from_locale_symbol("value"));

    hans_parameter item;
    item.id = scm_to_int(id);
    item.name = scm_to_hans_hash(name);
    item.size = scm_to_int(size);
    // item.offset = scm_to_int(offset);
    result.push_back(item);
  }

  return write_list<hans_parameter>(writer, HANS_BLOB_PARAMETERS, result);
}

static SCM write_parameter_values(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_parameter_value> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));
    result.push_back(scm_to_double(item));
  }

  return write_list<hans_parameter_value>(writer, HANS_BLOB_PARAMETER_VALUES,
                                          result);
}

static SCM write_programs(SCM writer, SCM lst, SCM nodata, SCM edge_nodata) {
  auto len = lst_length(lst);
  std::vector<hans_program> result;
  result.reserve(len);

  auto nodata_value = scm_to_int(nodata);
  auto edge_nodata_value = scm_to_int(edge_nodata);

  auto sym_name = scm_from_locale_symbol("name");
  auto sym_audio = scm_from_locale_symbol("audio");
  auto sym_graphics = scm_from_locale_symbol("graphics");

  auto zero = scm_from_int(0);
  auto one = scm_from_int(1);
  auto two = scm_from_int(2);
  auto three = scm_from_int(3);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto name = scm_assq_ref(alist, sym_name);
    auto audio = scm_assq_ref(alist, sym_audio);
    auto graphics = scm_assq_ref(alist, sym_graphics);

    hans_program item;
    item.name = scm_to_hans_hash(name);

    item.audio.node_start = nodata_value;
    item.audio.node_end = nodata_value;
    item.audio.edge_start = edge_nodata_value;
    item.audio.edge_end = edge_nodata_value;

    item.graphics.node_start = nodata_value;
    item.graphics.node_end = nodata_value;
    item.graphics.edge_start = edge_nodata_value;
    item.graphics.edge_end = edge_nodata_value;

    if (lst_length(audio) == 4) {
      item.audio.node_start = scm_to_int(scm_list_ref(audio, zero));
      item.audio.node_end = scm_to_int(scm_list_ref(audio, one));
      item.audio.edge_start = scm_to_int(scm_list_ref(audio, two));
      item.audio.edge_end = scm_to_int(scm_list_ref(audio, three));
    }

    if (lst_length(graphics) == 4) {
      item.graphics.node_start = scm_to_int(scm_list_ref(graphics, zero));
      item.graphics.node_end = scm_to_int(scm_list_ref(graphics, one));
      item.graphics.edge_start = scm_to_int(scm_list_ref(graphics, two));
      item.graphics.edge_end = scm_to_int(scm_list_ref(graphics, three));
    }

    result.push_back(item);
  }

  return write_list<hans_program>(writer, HANS_BLOB_PROGRAMS, result);
}

static SCM write_graphs(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<size_t> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto item = scm_list_ref(lst, scm_from_int(i));
    result.push_back(scm_to_int(item));
  }

  return write_list<size_t>(writer, HANS_BLOB_PROGRAM_GRAPHS, result);
}

static SCM write_graph_connections(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_object_connection> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto conn = scm_list_ref(lst, scm_from_int(i));
    hans_object_connection item;
    item.source = scm_to_int(scm_list_ref(conn, scm_from_int(0)));
    item.outlet = scm_to_int(scm_list_ref(conn, scm_from_int(1)));
    item.sink = scm_to_int(scm_list_ref(conn, scm_from_int(2)));
    item.inlet = scm_to_int(scm_list_ref(conn, scm_from_int(3)));
    result.push_back(item);
  }

  return write_list<hans_object_connection>(writer, HANS_BLOB_GRAPH_CONNECTIONS,
                                            result);
}

static SCM write_resources(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_resource_request> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto pair = scm_list_ref(lst, scm_from_int(i));

    hans_resource_request item;
    item.type = scm_to_hans_resource_type(scm_car(pair));
    item.amount = scm_to_int(scm_cdr(pair));
    result.push_back(item);
  }

  return write_list<hans_resource_request>(writer, HANS_BLOB_RESOURCE_REQUESTS,
                                           result);
}

static SCM write_strings(SCM writer, SCM lst) {
  auto len = lst_length(lst);

  std::vector<size_t> lengths;
  lengths.reserve(len);

  std::vector<size_t> offsets;
  offsets.reserve(len);

  std::vector<hans_hash> hashes;
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

  common::LinearAllocator allocator(sum);

  for (auto i = 0; i < len; ++i) {
    auto str = scm_list_ref(lst, scm_from_int(i));
    auto src = scm_to_locale_string(str);
    auto size = lengths.at(i);
    auto dest = allocator.allocate(size);
    std::memcpy(dest, static_cast<void*>(src), size);
    std::free(src);
  }

  auto hashes_size = sizeof(hans_hash) * len;
  auto offsets_size = sizeof(size_t) * len;

  auto instance = scm_to_writer(writer);
  instance->stage(HANS_BLOB_STRING_HASHES, static_cast<void*>(&hashes[0]),
                  hashes_size);
  instance->stage(HANS_BLOB_STRING_OFFSETS, static_cast<void*>(&offsets[0]),
                  offsets_size);
  instance->stage(HANS_BLOB_STRINGS, allocator.start(), sum);
  return scm_from_int(sum + hashes_size + offsets_size);
}

static SCM write_shaders(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_shader> result;
  result.reserve(len);

  auto sym_type = scm_from_locale_symbol("type");
  auto sym_name = scm_from_locale_symbol("name");
  auto sym_code = scm_from_locale_symbol("code");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto type = scm_assq_ref(alist, sym_type);
    auto name = scm_assq_ref(alist, sym_name);
    auto code = scm_assq_ref(alist, sym_code);

    hans_shader item;
    item.type = scm_to_hans_shader_type(type);
    item.uri = scm_to_hans_hash(name);
    item.code = scm_to_hans_hash(code);
    result.push_back(item);
  }

  return write_list<hans_shader>(writer, HANS_BLOB_SHADERS, result);
}

static SCM write_fbos(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_fbo> result;
  result.reserve(len);

  auto sym_stencil_buffer = scm_from_locale_symbol("stencil-buffer");
  auto sym_attachments = scm_from_locale_symbol("attachments");
  auto sym_instance_id = scm_from_locale_symbol("instance-id");

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto id = scm_assq_ref(alist, sym_instance_id);
    auto stencil = scm_assq_ref(alist, sym_stencil_buffer);
    auto attachments = scm_assq_ref(alist, sym_attachments);

    hans_fbo item;
    item.id = 0;
    item.object_id = scm_to_int(id);
    item.stencil_buffer = scm_to_bool(stencil);
    item.attachments = nullptr;
    item.num_attachments = 0;
    item.start = scm_to_int(scm_list_ref(attachments, scm_from_int(0)));
    item.end = scm_to_int(scm_list_ref(attachments, scm_from_int(1)));
    result.push_back(item);
  }

  return write_list<hans_fbo>(writer, HANS_BLOB_FBOS, result);
}

static hans_fbo_attachment_type scm_to_fbo_attachment_type(SCM type) {
  if (str_eqaul_sym("color", type)) {
    return HANS_COLOR_ATTACHMENT;
  } else if (str_eqaul_sym("depth", type)) {
    return HANS_DEPTH_ATTACHMENT;
  } else if (str_eqaul_sym("stencil", type)) {
    return HANS_STENCIL_ATTACHMENT;
  }

  throw std::runtime_error("Unknown fbo attachment type");
}

static SCM write_fbo_attachments(SCM writer, SCM lst) {
  auto len = lst_length(lst);
  std::vector<hans_fbo_attachment> result;
  result.reserve(len);

  for (auto i = 0; i < len; ++i) {
    auto alist = scm_list_ref(lst, scm_from_int(i));
    auto type = scm_assq_ref(alist, scm_from_locale_symbol("type"));
    auto width = scm_assq_ref(alist, scm_from_locale_symbol("width"));
    auto height = scm_assq_ref(alist, scm_from_locale_symbol("height"));
    auto components = scm_assq_ref(alist, scm_from_locale_symbol("components"));

    hans_fbo_attachment item;
    item.fbo_id = 0;
    item.type = scm_to_fbo_attachment_type(type);
    item.index = 0;
    item.width = scm_to_int(width);
    item.height = scm_to_int(height);
    item.components = scm_to_int(components);
    result.push_back(item);
  }

  return write_list<hans_fbo_attachment>(writer, HANS_BLOB_FBO_ATTACHMENTS,
                                         result);
}

static SCM make_hans_file_writer(SCM size) {
  void* place = scm_gc_malloc(sizeof(common::DataWriter), "hans-file-writer");
  auto writer = new (place) common::DataWriter(scm_to_int(size));
  return scm_new_smob(data_writer_tag, (scm_t_bits)writer);
}

static SCM hans_file_write(SCM writer, SCM dest) {
  scm_assert_smob_type(data_writer_tag, writer);
  auto instance = reinterpret_cast<common::DataWriter*>(SCM_SMOB_DATA(writer));
  auto str = scm_to_locale_string(dest);
  instance->write(str);
  std::free(str);
  return SCM_BOOL_T;
}

static SCM hans_resource_type_to_scm(hans_resource_type t) {
  switch (t) {
  case HANS_PARAMETER:
    return scm_from_locale_symbol("parameter");
  case HANS_SHADER:
    return scm_from_locale_symbol("shader");
  case HANS_AUDIO_BUFFER:
    return scm_from_locale_symbol("audio-buffer");
  case HANS_FRAME_BUFFER:
    return scm_from_locale_symbol("fbo");
  case HANS_INLET:
    return scm_from_locale_symbol("inlet");
  case HANS_OUTLET:
    return scm_from_locale_symbol("outlet");
  }

  throw std::runtime_error("Unknown resource type");
}

static hans_arg_type scm_to_hans_arg_type(SCM value) {
  if (scm_to_bool(scm_boolean_p(value)) == 1) {
    return HANS_BOOL;
  } else if (scm_to_bool(scm_number_p(value)) == 1) {
    return HANS_NUMBER;
  } else if (scm_to_bool(scm_string_p(value)) == 1) {
    return HANS_STRING;
  }

  throw std::runtime_error("Unknown argument type");
}

static SCM valid_shader(SCM type, SCM code) {
  return SCM_BOOL_T;
}

typedef struct {
  // Resource requests
  std::vector<SCM> requests;
  // All object arguments
  hans_user_arg* args;
  // Object index into args array
  std::vector<int> arg_offsets;
  std::vector<int> arg_lengths;
  // Current object being created
  int current_object;
} object_info_data;

static std::vector<hans_library> scm_to_hans_libs(
    SCM libraries, common::StringManager& strings) {
  int len = lst_length(libraries);
  std::vector<hans_library> result;
  result.reserve(len);

  for (int i = 0; i < len; ++i) {
    hans_library lib;
    auto path = scm_to_locale_string(scm_list_ref(libraries, scm_from_int(i)));
    lib.filepath = strings.intern(path);
    std::free(path);
    result.push_back(lib);
  }

  return result;
}

/// Fill in result with num args
static void scm_to_hans_obj_args(SCM args, common::StringManager& strings,
                                 hans_user_arg* out, int len) {
  for (int i = 0; i < len; ++i) {
    auto arg = scm_list_ref(args, scm_from_int(i));
    auto key = scm_car(arg);
    auto value = scm_cdr(arg);

    char* str = scm_to_locale_string(scm_symbol_to_string(key));

    hans_user_arg hans_arg;
    hans_arg.name = strings.intern(str);
    hans_arg.type = scm_to_hans_arg_type(value);

    switch (hans_arg.type) {
    case HANS_BOOL:
      hans_arg.boolean = scm_to_bool(value) == 1;
      break;
    case HANS_NUMBER:
      hans_arg.number = scm_to_double(value);
      break;
    case HANS_STRING:
      hans_arg.string = strings.intern(scm_to_locale_string(value));
      break;
    }

    out[i] = hans_arg;
  }
}

static std::vector<hans_object> scm_to_hans_objs(
    SCM objects, common::StringManager& strings) {
  int len = lst_length(objects);
  std::vector<hans_object> result;
  result.reserve(len);

  auto key_name = scm_from_locale_symbol("name");

  for (int i = 0; i < len; ++i) {
    auto user_object = scm_list_ref(objects, scm_from_int(i));

    hans_object obj;
    obj.size = 0;
    obj.make = nullptr;

    auto name = scm_to_locale_string(scm_assq_ref(user_object, key_name));
    obj.name = strings.intern(name);
    std::free(name);

    result.push_back(obj);
  }

  return result;
}

/// Return a pointer to the start of the objects arguments array
static hans_object_arguments get_args(hans_constructor_api* api) {
  auto info = static_cast<object_info_data*>(api->data);

  auto offset = info->arg_offsets.at(info->current_object);
  auto length = info->arg_lengths.at(info->current_object);

  hans_object_arguments out;
  out.data = &info->args[offset];
  out.length = length;
  return out;
}

/// Signal back to the callee that the object is missing a required argument
static void missing_required_arg(hans_constructor_api* api, hans_hash name,
                                 hans_arg_type type) {
  // TODO: Currently unused
}

static void request_resource(hans_constructor_api* api, hans_resource_type type,
                             int32_t amount) {
  auto info = static_cast<object_info_data*>(api->data);
  info->requests.push_back(
      scm_cons(hans_resource_type_to_scm(type), scm_from_int(amount)));
}

/// Returns an alist for each object instance describing its runtime resources
static SCM get_object_info(SCM libraries, SCM objects) {
  common::StringManager strings(16384 /* 16kb */);

  std::vector<hans_library> libs = scm_to_hans_libs(libraries, strings);
  std::vector<hans_object> objs = scm_to_hans_objs(objects, strings);

  object_info_data info;
  info.current_object = 0;
  info.requests.reserve(6); // No. of resource types
  info.arg_offsets.reserve(objs.size());
  info.arg_lengths.reserve(objs.size());

  auto total_num_args = 0;
  auto arg_key = scm_from_locale_symbol("args");
  for (int i = 0; i < lst_length(objects); ++i) {
    auto obj = scm_list_ref(objects, scm_from_int(i));
    auto args = scm_assq_ref(obj, arg_key);

    // FIXME: Check that its not #f
    auto len = lst_length(args);

    info.arg_offsets.push_back(total_num_args);
    info.arg_lengths.push_back(len);
    total_num_args += len;
  }

  info.args = new hans_user_arg[total_num_args];

  for (int i = 0; i < lst_length(objects); ++i) {
    auto num_args = info.arg_lengths.at(i);
    if (num_args == 0) {
      continue;
    }

    auto obj = scm_list_ref(objects, scm_from_int(i));
    auto args = scm_assq_ref(obj, arg_key);

    auto out = &info.args[info.arg_offsets.at(i)];
    scm_to_hans_obj_args(args, strings, out, num_args);
  }

  engine::LibraryManager library_manager(strings, objs);
  library_manager.load_libraries(libs);

  auto total_objects_bytes = 0;
  for (auto& obj : objs) {
    assert(obj.make != nullptr);
    assert(obj.init != nullptr);
    assert(obj.size != 0);
    total_objects_bytes += obj.size;
  }

  common::LinearAllocator allocator(total_objects_bytes);

  hans_constructor_api constructor_api;
  constructor_api.get_args = get_args;
  constructor_api.missing_required_arg = missing_required_arg;
  constructor_api.request_resource = request_resource;
  constructor_api.data = &info;

  SCM out = SCM_EOL;

  for (auto& obj : objs) {
    auto buff = static_cast<char*>(allocator.allocate(obj.size));
    obj.make(&constructor_api, buff, obj.size);

    SCM lst = SCM_EOL;
    for (auto& req : info.requests) {
      lst = scm_cons(req, lst);
    }

    // Copy object data
    auto bv = scm_c_make_bytevector(obj.size);
    auto dest = SCM_BYTEVECTOR_CONTENTS(bv);
    std::memcpy(dest, buff, obj.size);

    info.requests.clear();
    info.current_object += 1;
    out = scm_cons(scm_list_2(bv, lst), out);
  }

  delete[] info.args;
  return scm_reverse(out);
}

extern "C" {
void scm_init_hans_compiler_module() {
  scm_c_define_gsubr("make-hans-file-writer", 1, 0, 0,
                     (scm_t_subr)make_hans_file_writer);
  auto data_writer_size = sizeof(common::DataWriter);
  data_writer_tag = scm_make_smob_type("hans-file-writer", data_writer_size);
  scm_c_define_gsubr("hans-file-write", 2, 0, 0, (scm_t_subr)hans_file_write);

  scm_c_define_gsubr("write-libraries", 2, 0, 0, (scm_t_subr)write_libraries);
  scm_c_define_gsubr("write-objects", 2, 0, 0, (scm_t_subr)write_objects);
  scm_c_define_gsubr("write-object-data", 2, 0, 0,
                     (scm_t_subr)write_object_data);
  scm_c_define_gsubr("write-programs", 4, 0, 0, (scm_t_subr)write_programs);
  scm_c_define_gsubr("write-resources", 2, 0, 0, (scm_t_subr)write_resources);
  scm_c_define_gsubr("write-graphs", 2, 0, 0, (scm_t_subr)write_graphs);
  scm_c_define_gsubr("write-graph-connections", 2, 0, 0,
                     (scm_t_subr)write_graph_connections);
  scm_c_define_gsubr("write-parameters", 2, 0, 0, (scm_t_subr)write_parameters);
  scm_c_define_gsubr("write-parameter-values", 2, 0, 0,
                     (scm_t_subr)write_parameter_values);
  scm_c_define_gsubr("write-shaders", 2, 0, 0, (scm_t_subr)write_shaders);
  scm_c_define_gsubr("write-fbos", 2, 0, 0, (scm_t_subr)write_fbos);
  scm_c_define_gsubr("write-fbo-attachments", 2, 0, 0,
                     (scm_t_subr)write_fbo_attachments);
  scm_c_define_gsubr("write-strings", 2, 0, 0, (scm_t_subr)write_strings);

  scm_c_define_gsubr("valid-shader?", 2, 0, 0, (scm_t_subr)valid_shader);
  scm_c_define_gsubr("get-object-info", 2, 0, 0, (scm_t_subr)get_object_info);
}
}