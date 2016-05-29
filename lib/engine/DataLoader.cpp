#include "hans/engine/DataLoader.hpp"
#include <sqlite3.h>
#include <cassert>
#include <cstdlib>
#include <stdexcept>
#include <string>

using namespace hans;

engine::DataLoader::DataLoader(const char *path,
                               hans::common::StringManager &string_manager)
    : m_string_manager(string_manager) {
  sqlite3_open(path, &m_connection);
  assert(m_connection != nullptr);
}

engine::DataLoader::~DataLoader() {
  sqlite3_close(m_connection);
}

std::vector<hans_parameter> engine::DataLoader::get_parameters() const {
  auto query = "SELECT id, object_id, name, size FROM parameters;";

  sqlite3_stmt *st;
  const char *tail;

  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  // Retrieve all the parameters
  std::vector<hans_parameter> results;

  while (sqlite3_step(st) != SQLITE_DONE) {
    hans_parameter parameter;
    parameter.id = sqlite3_column_int(st, 0);
    parameter.object_id = sqlite3_column_int(st, 1);
    parameter.name = m_string_manager.intern(sqlite3_column_text(st, 2));
    parameter.size = sqlite3_column_int(st, 3);
    results.push_back(parameter);
  }

  sqlite3_finalize(st);

  for (auto &p : results) {
    if (p.size != 0) {
      auto s = p.size * sizeof(hans_parameter_value);
      p.values = static_cast<hans_parameter_value *>(std::calloc(1, s));
    }
  }

  // Fill in the default values
  query = "SELECT parameter_id, component, value FROM parameter_defaults;";
  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  while (sqlite3_step(st) != SQLITE_DONE) {
    hans_parameter_id id = sqlite3_column_int(st, 0);
    int component = sqlite3_column_int(st, 1);

    for (auto &parameter : results) {
      if (parameter.id == id) {
        parameter.values[component] = sqlite3_column_double(st, 2);
        break;
      }
    }
  }

  sqlite3_finalize(st);
  return results;
}

void engine::DataLoader::del_parameters(
    std::vector<hans_parameter> &parameters) const {
  for (auto &parameter : parameters) {
    std::free(parameter.values);
  }
}

std::vector<hans_fbo> engine::DataLoader::get_frame_buffers() const {
  auto query =
      "SELECT id, object_id, stencil_buffer, attachments FROM "
      "frame_buffers;";

  sqlite3_stmt *st;
  const char *tail;

  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  std::vector<hans_fbo> results;
  while (sqlite3_step(st) != SQLITE_DONE) {
    hans_fbo fbo;
    fbo.id = sqlite3_column_int(st, 0);
    fbo.object_id = sqlite3_column_int(st, 1);
    fbo.stencil_buffer = sqlite3_column_int(st, 2) > 0;
    fbo.num_attachments = sqlite3_column_int(st, 3);
    fbo.attachments = new hans_fbo_attachment[fbo.num_attachments];
    results.push_back(fbo);
  }

  query =
      "SELECT frame_buffer_id, type, attachment, width, height, components "
      "FROM frame_buffer_attachments ORDER BY frame_buffer_id DESC, attachment "
      "ASC;";
  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  int i = 0;
  int previous_id = -1;
  while (sqlite3_step(st) != SQLITE_DONE) {
    auto fbo_id = sqlite3_column_int(st, 0);
    auto type = sqlite3_column_int(st, 1);

    for (auto &frame_buffer : results) {
      if (frame_buffer.id != fbo_id) {
        continue;
      }

      if (previous_id != frame_buffer.id) {
        i = 0;
        previous_id = frame_buffer.id;
      }

      frame_buffer.attachments[i].fbo_id = fbo_id;

      switch (type) {
      case 0:
        frame_buffer.attachments[i].type = HANS_COLOR_ATTACHMENT;
        break;
      case 1:
        frame_buffer.attachments[i].type = HANS_DEPTH_ATTACHMENT;
        break;
      case 2:
        frame_buffer.attachments[i].type = HANS_STENCIL_ATTACHMENT;
        break;
      }

      frame_buffer.attachments[i].index = sqlite3_column_int(st, 2);
      frame_buffer.attachments[i].width = sqlite3_column_int(st, 3);
      frame_buffer.attachments[i].height = sqlite3_column_int(st, 4);
      frame_buffer.attachments[i].components = sqlite3_column_int(st, 5);
      i++;
    }
  }

  sqlite3_finalize(st);
  return results;
}

void engine::DataLoader::del_frame_buffers(
    std::vector<hans_fbo> &frame_buffers) const {
  for (auto &frame_buffer : frame_buffers) {
    delete[] frame_buffer.attachments;
  }
}

std::vector<hans_library> engine::DataLoader::get_libraries() const {
  auto query = "SELECT name, filepath FROM libraries;";

  sqlite3_stmt *st;
  const char *tail;

  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  std::vector<hans_library> results;
  while (sqlite3_step(st) != SQLITE_DONE) {
    results.push_back(
        {.name = m_string_manager.intern(sqlite3_column_text(st, 0)),
         .filepath = m_string_manager.intern(sqlite3_column_text(st, 1)),
         .handle = nullptr});
  }

  sqlite3_finalize(st);
  return results;
}

std::vector<hans_object> engine::DataLoader::get_objects() const {
  auto query = "SELECT id, name, type FROM objects;";

  sqlite3_stmt *st;
  const char *tail;

  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  std::vector<hans_object> results;

  while (sqlite3_step(st) != SQLITE_DONE) {
    hans_object object;

    switch (sqlite3_column_int(st, 2)) {
    // Object types defined in schema.sql
    case 0:
      object.type = HANS_AUDIO;
      break;
    case 1:
      object.type = HANS_GRAPHICS;
      break;
    default:
      throw std::runtime_error("Unknown object type");
    }

    object.id = sqlite3_column_int(st, 0);
    object.size = 0;
    object.make = nullptr;
    object.destroy = nullptr;
    object.name = m_string_manager.intern(sqlite3_column_text(st, 1));
    results.push_back(object);
  }

  sqlite3_finalize(st);
  return results;
}

std::vector<hans_shader> engine::DataLoader::get_shaders() const {
  auto query =
      "SELECT resources.id, resources.uri, shaders.type, shaders.code FROM "
      "resources INNER JOIN shaders ON resources.id=shaders.resource_id;";

  sqlite3_stmt *st;
  const char *tail;

  if (sqlite3_prepare_v2(m_connection, query, -1, &st, &tail) != SQLITE_OK) {
    throw std::runtime_error("Bad query");
  }

  std::vector<hans_shader> results;

  while (sqlite3_step(st) != SQLITE_DONE) {
    hans_shader shader;
    shader.uri = m_string_manager.intern(sqlite3_column_text(st, 1));

    switch (sqlite3_column_int(st, 2)) {
    // Shader types defined in schema.sql
    case 0:
      shader.type = HANS_SHADER_VERTEX;
      break;
    case 1:
      shader.type = HANS_SHADER_FRAGMENT;
      break;
    default:
      throw std::runtime_error("Unknown shader type");
    }

    shader.code = m_string_manager.intern(sqlite3_column_text(st, 3));
    results.push_back(shader);
  }

  sqlite3_finalize(st);
  return results;
}
