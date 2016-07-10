#include "hans/common/DataLoader.hpp"
#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>

using namespace hans;

common::DataWriter::DataWriter(size_t size) : m_allocator(size) {
  m_cleanup = false;
}

common::DataWriter::~DataWriter() {
  if (m_cleanup) {
    std::free(m_file.data);
  }
}

bool common::DataWriter::stage(hans_blob_type type, void* data, size_t size) {
  auto len = m_blobs.size();

  hans_blob blob;
  blob.type = type;
  blob.data = m_allocator.allocate(size);
  blob.size = size;

  assert(blob.data != nullptr);
  std::memcpy(blob.data, data, size);

  if (len == 0) {
    blob.offset = 0;
  } else {
    auto prev = m_blobs.at(len - 1);
    blob.offset = prev.offset + prev.size;
  }

  m_blobs.push_back(blob);
  return true;
}

bool common::DataWriter::write(const char* uri) {
  auto size = 0;
  for (const auto& blob : m_blobs) {
    size += blob.size;
  }

  m_file.version = 1;
  m_file.little_endian = true;
  m_file.length = m_blobs.size();
  m_file.data = m_allocator.start();
  m_file.size = size;
  m_file.blobs = &m_blobs[0];

  std::ofstream stream(uri, std::ios::binary);
  stream.write(reinterpret_cast<char*>(&m_file), sizeof(hans_file));
  stream.write(reinterpret_cast<char*>(m_file.blobs),
               sizeof(hans_blob) * m_file.length);
  stream.write(reinterpret_cast<char*>(m_file.data), m_file.size);
  stream.close();

  std::free(m_file.data);
  m_cleanup = false;
  return true;
}

common::DataReader::DataReader(const char* uri) {
  std::ifstream stream(uri, std::ios::binary);
  stream.read(reinterpret_cast<char*>(&file), sizeof(hans_file));

  file.blobs = new hans_blob[file.length];
  file.data = std::malloc(file.size);

  stream.read(reinterpret_cast<char*>(file.blobs),
              sizeof(hans_blob) * file.length);
  stream.read(reinterpret_cast<char*>(file.data), file.size);
  stream.close();

  auto base = static_cast<char*>(file.data);
  for (auto i = 0; i < file.length; ++i) {
    auto blob = file.blobs[i];
    blob.data = base + blob.offset;

    switch (blob.type) {
    case HANS_BLOB_STRINGS:
      data.strings = ListView<const char>(blob);
      break;
    case HANS_BLOB_STRING_OFFSETS:
      data.string_offsets = ListView<size_t>(blob);
      break;
    case HANS_BLOB_STRING_HASHES:
      data.string_hashes = ListView<hans_hash>(blob);
      break;
    case HANS_BLOB_LIBRARIES:
      data.libraries = ListView<hans_library>(blob);
      break;
    case HANS_BLOB_OBJECTS:
      data.objects = ListView<hans_object>(blob);
      break;
    case HANS_BLOB_OBJECTS_DATA:
      data.object_data = blob;
      break;
    case HANS_BLOB_PARAMETERS:
      data.parameters = ListView<hans_parameter>(blob);
      break;
    case HANS_BLOB_PARAMETER_VALUES:
      data.parameter_values = ListView<hans_parameter_value>(blob);
      break;
    case HANS_BLOB_PROGRAMS:
      data.programs = ListView<hans_program>(blob);
      break;
    case HANS_BLOB_CHAINS:
      data.chains = ListView<size_t>(blob);
      break;
    case HANS_BLOB_REGISTERS:
      data.registers = ListView<hans_register>(blob);
      break;
    case HANS_BLOB_SHADERS:
      data.shaders = ListView<hans_shader>(blob);
      break;
    case HANS_BLOB_FBOS:
      data.fbos = ListView<hans_fbo>(blob);
      break;
    case HANS_BLOB_FBO_ATTACHMENTS:
      data.fbo_attachments = ListView<hans_fbo_attachment>(blob);
      break;
    case HANS_BLOB_AUDIO_BUFFERS:
      data.audio_buffers = ListView<hans_audio_buffer>(blob);
      break;
    case HANS_BLOB_RING_BUFFERS:
      data.ring_buffers = ListView<hans_ring_buffer>(blob);
      break;
    case HANS_BLOB_MODULATORS:
      data.modulators = ListView<hans_modulator>(blob);
      break;
    }
  }
}

common::DataReader::~DataReader() {
  delete[] file.blobs;
  std::free(file.data);
}
