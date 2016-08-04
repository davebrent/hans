#ifndef HANS_COMMON_DATALOADER_H_
#define HANS_COMMON_DATALOADER_H_

#include <vector>
#include "hans/audio/types.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"
#include "hans/graphics/types.hpp"

namespace hans {
namespace common {

class DataWriter {
 public:
  explicit DataWriter(size_t size);
  ~DataWriter();
  /// Stage some data for writing
  bool stage(DataFile::Types type, void* data, size_t size);
  /// Write staged data to disk
  bool write(const char* uri);

 private:
  DataFile m_file;
  std::vector<DataFile::Blob> m_blobs;
  LinearAllocator m_allocator;
  bool m_cleanup;
};

class DataReader {
 public:
  struct Inflated {
    ListView<const char> strings;
    ListView<hash> string_hashes;
    ListView<size_t> string_offsets;
    ListView<engine::Library> libraries;
    ListView<engine::ObjectDef> objects;
    DataFile::Blob object_data;
    ListView<engine::Parameter> parameters;
    ListView<engine::Parameter::Value> parameter_values;
    ListView<engine::Program> programs;
    ListView<size_t> chains;
    ListView<engine::Register> registers;
    ListView<graphics::Shader> shaders;
    ListView<graphics::FBO> fbos;
    ListView<graphics::FBO::Attachment> fbo_attachments;
    ListView<audio::Buffer> audio_buffers;
    ListView<audio::RingBuffer> ring_buffers;
    ListView<engine::Modulator> modulators;
  };

  explicit DataReader(const char* uri);
  ~DataReader();
  DataFile file;
  Inflated data;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_DATALOADER_H_
