#ifndef HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
#define HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_

#include <vector>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"
#include "hans/graphics/types.hpp"

namespace hans {
namespace graphics {

class FrameBufferManager {
 public:
  FrameBufferManager(common::ListView<FBO> fbos,
                     common::ListView<FBO::Attachment> attachments);
  ~FrameBufferManager();
  void setup();

  /// Create an objects frame buffer and corresponding attachments
  FBO make(engine::ObjectDef::ID object);

  /// Bind to the windows default frame buffer
  /// Calls glBindFramebuffer on the requested frame buffer
  void bind_fbo(const FBO& fbo) const;
  void release_fbo() const;

  uint32_t get_color_attachment(const FBO& handle, uint16_t i) const;
  uint32_t get_depth_attachment(const FBO& handle) const;
  uint32_t get_stencil_attachment(const FBO& handle) const;

  /// Calls glBindTexture for the requested color attachment
  /// Calls glBindTexture for the depth attachment of the frame buffer
  /// Calls glBindTexture for the stencil attachment of the frame buffer
  void bind_color_attachment(const FBO& handle, uint16_t i) const;
  void bind_depth_attachment(const FBO& handle) const;
  void bind_stencil_attachment(const FBO& handle) const;

 private:
  common::ListView<FBO> m_fbos;
  common::ListView<FBO::Attachment> m_attachments;
  uint32_t* m_gl_fbos;
  uint32_t* m_gl_attachments;
};

} // namespace graphics
} // namespace hans

#endif // HANS_GRAPHICS_FRAMEBUFFERMANAGER_H_
