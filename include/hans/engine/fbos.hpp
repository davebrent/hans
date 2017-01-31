#ifndef HANS_ENGINE_FBOS_H_
#define HANS_ENGINE_FBOS_H_

#include <vector>
#include "hans/engine/primitives.hpp"

namespace hans {
namespace engine {

class FrameBufferManager {
 public:
  FrameBufferManager(const FrameBufferManager& other) = delete;
  FrameBufferManager(std::vector<graphics::FBO>& fbos,
                     std::vector<graphics::FBO::Attachment>& attachments);
  ~FrameBufferManager();
  void setup();
  void destroy();

  /// Create an objects frame buffer and corresponding attachments
  graphics::FBO make(ObjectDef::ID object);

  /// Bind to the windows default frame buffer
  /// Calls glBindFramebuffer on the requested frame buffer
  void bind_fbo(const graphics::FBO& fbo) const;
  void release_fbo() const;

  uint32_t get_color_attachment(const graphics::FBO& handle, uint16_t i) const;
  uint32_t get_depth_attachment(const graphics::FBO& handle) const;
  uint32_t get_stencil_attachment(const graphics::FBO& handle) const;

  /// Calls glBindTexture for the requested color attachment
  /// Calls glBindTexture for the depth attachment of the frame buffer
  /// Calls glBindTexture for the stencil attachment of the frame buffer
  void bind_color_attachment(const graphics::FBO& handle, uint16_t i) const;
  void bind_depth_attachment(const graphics::FBO& handle) const;
  void bind_stencil_attachment(const graphics::FBO& handle) const;

 private:
  std::vector<graphics::FBO>& m_fbos;
  std::vector<graphics::FBO::Attachment>& m_attachments;
  uint32_t* m_gl_fbos;
  uint32_t* m_gl_attachments;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_FBOS_H_
