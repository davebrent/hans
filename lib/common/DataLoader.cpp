#include "hans/common/DataLoader.hpp"
#include <cstring>
#include <fstream>
#include <iostream>
#include <vector>

using namespace hans;

common::DataWriter::DataWriter(size_t size) : m_allocator(size) {
  m_cleanup = false;
}

common::DataWriter::~DataWriter() {
  if (m_cleanup) {
    delete[] m_file.blobs;
    std::free(m_file.data);
  }
}

bool common::DataWriter::stage(hans_blob_type type, void* data, size_t size) {
  auto len = m_blobs.size();

  hans_blob blob;
  blob.type = type;
  blob.data = m_allocator.allocate(size);
  blob.size = size;

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

  delete[] m_file.blobs;
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
    file.blobs[i].data = base + file.blobs[i].offset;
  }
}

common::DataReader::~DataReader() {
  delete[] file.blobs;
  std::free(file.data);
}
