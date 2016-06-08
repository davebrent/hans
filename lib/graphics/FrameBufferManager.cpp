#include "hans/graphics/FrameBufferManager.hpp"
#include <stdexcept>
#include "hans/graphics/gl.h"

using namespace hans;

graphics::FrameBufferManager::FrameBufferManager(
    common::ListView<hans_fbo>& fbos,
    common::ListView<hans_fbo_attachment>& attachments) {
  m_fbos = &fbos[0];
  m_fbos_length = fbos.size();
  m_attachments = &attachments[0];
  m_attachments_len = attachments.size();

  m_gl_fbos = new uint32_t[m_fbos_length];
  m_gl_attachments = new uint32_t[m_attachments_len];

  glGenFramebuffers(m_fbos_length, m_gl_fbos);
  glGenTextures(m_attachments_len, m_gl_attachments);

  GLenum color_attachments[8] = {GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1,
                                 GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3,
                                 GL_COLOR_ATTACHMENT4, GL_COLOR_ATTACHMENT5,
                                 GL_COLOR_ATTACHMENT6, GL_COLOR_ATTACHMENT7};

  auto fbo_index = 0;
  for (const auto& fbo_config : fbos) {
    // Setup all the attachments for the FBO
    glBindFramebuffer(GL_FRAMEBUFFER, m_gl_fbos[fbo_index]);
    fbo_index++;

    auto num_color_attachments = 0;

    // Create the attachments
    for (auto i = fbo_config.start; i < fbo_config.end; ++i) {
      auto& attachment_config = attachments[i];
      auto width = attachment_config.width;
      auto height = attachment_config.height;
      auto texture = m_gl_attachments[i];

      switch (attachment_config.type) {
      case HANS_COLOR_ATTACHMENT:
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA,
                     GL_UNSIGNED_BYTE, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER,
                             GL_COLOR_ATTACHMENT0 + num_color_attachments,
                             texture, 0);
        num_color_attachments++;
        break;

      case HANS_DEPTH_ATTACHMENT:
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0,
                     GL_DEPTH_COMPONENT, GL_FLOAT, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, texture, 0);
        break;

      case HANS_STENCIL_ATTACHMENT:
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA,
                     GL_UNSIGNED_BYTE, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, texture, 0);

        break;
      }
    }

    // Defines the array of buffers into which outputs from the fragment shader
    // data will be written to
    glDrawBuffers(num_color_attachments, color_attachments);
    // Check that the FBO is complete
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
      throw std::runtime_error("FBO Incomplete");
    }
  }
}

graphics::FrameBufferManager::~FrameBufferManager() {
  glDeleteFramebuffers(m_fbos_length, m_gl_fbos);
  glDeleteTextures(m_attachments_len, m_gl_attachments);
  delete[] m_gl_fbos;
  delete[] m_gl_attachments;
}

hans_fbo graphics::FrameBufferManager::make(hans_instance_id object) {
  auto fbos = m_fbos;
  for (auto i = 0; i < m_fbos_length; ++i) {
    auto fbo = fbos[i];
    if (fbo.object == object) {
      return fbo;
    }
  }
  throw std::runtime_error("Unable to find object");
}

void graphics::FrameBufferManager::release_fbo() const {
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void graphics::FrameBufferManager::bind_fbo(const hans_fbo& fbo) const {
  auto fbos = m_fbos;
  for (auto i = 0; i < m_fbos_length; ++i) {
    if (fbos[i].object == fbo.object) {
      glBindFramebuffer(GL_FRAMEBUFFER, m_gl_fbos[i]);
      return;
    }
  }
}

uint32_t graphics::FrameBufferManager::get_color_attachment(
    const hans_fbo& fbo, uint16_t index) const {
  auto seen = 0;
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == HANS_COLOR_ATTACHMENT) {
      if (seen == index) {
        return m_gl_attachments[i];
      }
      seen++;
    }
  }
  return 0;
}

uint32_t graphics::FrameBufferManager::get_depth_attachment(
    const hans_fbo& fbo) const {
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == HANS_DEPTH_ATTACHMENT) {
      return m_gl_attachments[i];
    }
  }
  return 0;
}

uint32_t graphics::FrameBufferManager::get_stencil_attachment(
    const hans_fbo& fbo) const {
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == HANS_STENCIL_ATTACHMENT) {
      return m_gl_attachments[i];
    }
  }
  return 0;
}

void graphics::FrameBufferManager::bind_color_attachment(const hans_fbo& fbo,
                                                         uint16_t index) const {
  glBindTexture(GL_TEXTURE_2D, get_color_attachment(fbo, index));
}

void graphics::FrameBufferManager::bind_depth_attachment(
    const hans_fbo& fbo) const {
  glBindTexture(GL_TEXTURE_2D, get_depth_attachment(fbo));
}

void graphics::FrameBufferManager::bind_stencil_attachment(
    const hans_fbo& fbo) const {
  glBindTexture(GL_TEXTURE_2D, get_stencil_attachment(fbo));
}
