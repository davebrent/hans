#ifndef HANS_COMMON_DATALOADER_H_
#define HANS_COMMON_DATALOADER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace common {

class DataWriter {
 public:
  explicit DataWriter(size_t size);
  ~DataWriter();
  /// Stage some data for writing
  bool stage(hans_blob_type type, void* data, size_t size);
  /// Write staged data to disk
  bool write(const char* uri);

 private:
  hans_file m_file;
  std::vector<hans_blob> m_blobs;
  hans::common::LinearAllocator m_allocator;
  bool m_cleanup;
};

class DataReader {
 public:
  DataReader(const char* uri);
  ~DataReader();
  hans_file file;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_DATALOADER_H_
