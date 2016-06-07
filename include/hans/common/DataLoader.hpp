#ifndef HANS_COMMON_DATALOADER_H_
#define HANS_COMMON_DATALOADER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
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
  typedef struct {
    ListView<const char> strings;
    ListView<hans_hash> string_hashes;
    ListView<size_t> string_offsets;
    ListView<hans_library> libraries;
    ListView<hans_object> objects;
    hans_blob object_data;
    ListView<hans_parameter> parameters;
    ListView<hans_parameter_value> parameter_values;
    ListView<hans_program> programs;
    ListView<size_t> chains;
    ListView<hans_register> registers;
    ListView<hans_resource_request> resource_requests;
    ListView<hans_shader> shaders;
    ListView<hans_fbo> fbos;
    ListView<hans_fbo_attachment> fbo_attachments;
  } file_data;

  explicit DataReader(const char* uri);
  ~DataReader();
  hans_file file;
  file_data data;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_DATALOADER_H_
