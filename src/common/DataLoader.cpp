#include "hans/common/DataLoader.hpp"
#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

DataWriter::DataWriter(size_t size) : m_allocator(size) {
  m_cleanup = false;
}

DataWriter::~DataWriter() {
  if (m_cleanup) {
    std::free(m_file.data);
  }
}

bool DataWriter::stage(DataFile::Types type, void* data, size_t size) {
  auto len = m_blobs.size();

  DataFile::Blob blob;
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

bool DataWriter::write(const char* uri) {
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
  stream.write(reinterpret_cast<char*>(&m_file), sizeof(DataFile));
  stream.write(reinterpret_cast<char*>(m_file.blobs),
               sizeof(DataFile::Blob) * m_file.length);
  stream.write(reinterpret_cast<char*>(m_file.data), m_file.size);
  stream.close();

  std::free(m_file.data);
  m_cleanup = false;
  return true;
}

DataReader::DataReader(const char* uri) {
  std::ifstream stream(uri, std::ios::binary);
  stream.read(reinterpret_cast<char*>(&file), sizeof(DataFile));

  file.blobs = new DataFile::Blob[file.length];
  file.data = std::malloc(file.size);

  stream.read(reinterpret_cast<char*>(file.blobs),
              sizeof(DataFile::Blob) * file.length);
  stream.read(reinterpret_cast<char*>(file.data), file.size);
  stream.close();

  auto base = static_cast<char*>(file.data);
  for (auto i = 0; i < file.length; ++i) {
    auto blob = file.blobs[i];
    blob.data = base + blob.offset;

    switch (blob.type) {
    case DataFile::Types::STRINGS:
      data.strings = ListView<const char>(blob);
      break;
    case DataFile::Types::STRING_OFFSETS:
      data.string_offsets = ListView<size_t>(blob);
      break;
    case DataFile::Types::STRING_HASHES:
      data.string_hashes = ListView<hash>(blob);
      break;
    case DataFile::Types::LIBRARIES:
      data.libraries = ListView<Library>(blob);
      break;
    case DataFile::Types::OBJECTS:
      data.objects = ListView<ObjectDef>(blob);
      break;
    case DataFile::Types::OBJECTS_DATA:
      data.object_data = blob;
      break;
    case DataFile::Types::PARAMETERS:
      data.parameters = ListView<Parameter>(blob);
      break;
    case DataFile::Types::PARAMETER_VALUES:
      data.parameter_values = ListView<Parameter::Value>(blob);
      break;
    case DataFile::Types::PROGRAMS:
      data.programs = ListView<Program>(blob);
      break;
    case DataFile::Types::CHAINS:
      data.chains = ListView<size_t>(blob);
      break;
    case DataFile::Types::REGISTERS:
      data.registers = ListView<Register>(blob);
      break;
    case DataFile::Types::SHADERS:
      data.shaders = ListView<Shader>(blob);
      break;
    case DataFile::Types::FBOS:
      data.fbos = ListView<FBO>(blob);
      break;
    case DataFile::Types::FBO_ATTACHMENTS:
      data.fbo_attachments = ListView<FBO::Attachment>(blob);
      break;
    case DataFile::Types::AUDIO_BUFFERS:
      data.audio_buffers = ListView<Buffer>(blob);
      break;
    case DataFile::Types::RING_BUFFERS:
      data.ring_buffers = ListView<RingBuffer>(blob);
      break;
    case DataFile::Types::MODULATORS:
      data.modulators = ListView<Modulator>(blob);
      break;
    }
  }
}

DataReader::~DataReader() {
  delete[] file.blobs;
  std::free(file.data);
}
