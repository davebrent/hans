#ifndef HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
#define HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_

#include <vector>
#include "hans/common/types.hpp"

namespace hans {
namespace graphics {

class FrameBufferManager {
 public:
  FrameBufferManager(const std::vector<hans_fbo>& defaults);
  ~FrameBufferManager();

  /// Return the number of handles required for a list of object id's
  uint16_t set_objects(const std::vector<hans_object_id>& object_ids);

  /// Create the frame buffer for an object
  int make(hans_object_resource* resources, const hans_object_id object_id,
           const hans_instance_id instance_id);

  /// Bind to the windows default frame buffer
  /// Calls glBindFramebuffer on the requested frame buffer
  void bind_frame_buffer(const hans_fbo_handle& handle) const;
  void release_frame_buffer() const;

  uint32_t get_color_attachment(const hans_fbo_handle& handle,
                                uint16_t i) const;
  uint32_t get_depth_attachment(const hans_fbo_handle& handle) const;
  uint32_t get_stencil_attachment(const hans_fbo_handle& handle) const;

  /// Calls glBindTexture for the requested color attachment
  /// Calls glBindTexture for the depth attachment of the frame buffer
  /// Calls glBindTexture for the stencil attachment of the frame buffer
  void bind_color_attachment(const hans_fbo_handle& handle, uint16_t i) const;
  void bind_depth_attachment(const hans_fbo_handle& handle) const;
  void bind_stencil_attachment(const hans_fbo_handle& handle) const;

 private:
  void set_capacity(uint16_t num_frame_buffers, uint16_t num_attachments);

  uint32_t* m_frame_buffers;
  uint32_t* m_attachments;

  uint32_t m_num_frame_buffers;
  uint32_t m_num_attachments;

  uint32_t m_offset_frame_buffers;
  uint32_t m_offset_attachments;

  const std::vector<hans_fbo>& m_frame_buffer_defaults;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
