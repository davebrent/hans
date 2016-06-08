#ifndef HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
#define HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_

#include <vector>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace graphics {

class FrameBufferManager {
 public:
  FrameBufferManager(common::ListView<hans_fbo>& fbos,
                     common::ListView<hans_fbo_attachment>& attachments);
  ~FrameBufferManager();

  /// Create an objects frame buffer and corresponding attachments
  hans_fbo make(hans_instance_id object);

  /// Bind to the windows default frame buffer
  /// Calls glBindFramebuffer on the requested frame buffer
  void bind_fbo(const hans_fbo& fbo) const;
  void release_fbo() const;

  uint32_t get_color_attachment(const hans_fbo& handle, uint16_t i) const;
  uint32_t get_depth_attachment(const hans_fbo& handle) const;
  uint32_t get_stencil_attachment(const hans_fbo& handle) const;

  /// Calls glBindTexture for the requested color attachment
  /// Calls glBindTexture for the depth attachment of the frame buffer
  /// Calls glBindTexture for the stencil attachment of the frame buffer
  void bind_color_attachment(const hans_fbo& handle, uint16_t i) const;
  void bind_depth_attachment(const hans_fbo& handle) const;
  void bind_stencil_attachment(const hans_fbo& handle) const;

 private:
  hans_fbo* m_fbos;
  hans_fbo_attachment* m_attachments;
  size_t m_fbos_length;
  size_t m_attachments_len;
  uint32_t* m_gl_fbos;
  uint32_t* m_gl_attachments;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
