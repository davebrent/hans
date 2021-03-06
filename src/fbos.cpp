#include "hans/fbos.hpp"
#include <iostream>
#include <stdexcept>
#include "hans/gl.hpp"

using namespace hans;
using namespace hans::graphics;

FrameBufferManager::FrameBufferManager(
    std::vector<FBO>& fbos, std::vector<FBO::Attachment>& attachments)
    : m_fbos(fbos), m_attachments(attachments) {
  m_gl_fbos = new uint32_t[m_fbos.size()];
  m_gl_attachments = new uint32_t[m_attachments.size()];
}

void FrameBufferManager::setup() {
  glGenFramebuffers(m_fbos.size(), m_gl_fbos);
  glGenTextures(m_attachments.size(), m_gl_attachments);

  GLenum color_attachments[8] = {GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1,
                                 GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3,
                                 GL_COLOR_ATTACHMENT4, GL_COLOR_ATTACHMENT5,
                                 GL_COLOR_ATTACHMENT6, GL_COLOR_ATTACHMENT7};

  auto fbo_index = 0;
  for (const auto& fbo_config : m_fbos) {
    // Setup all the attachments for the FBO
    glBindFramebuffer(GL_FRAMEBUFFER, m_gl_fbos[fbo_index]);
    fbo_index++;

    auto num_color_attachments = 0;

    // Create the attachments
    for (auto i = fbo_config.start; i < fbo_config.end; ++i) {
      auto& attachment_config = m_attachments[i];
      auto width = attachment_config.width;
      auto height = attachment_config.height;
      auto texture = m_gl_attachments[i];

      switch (attachment_config.type) {
      case FBO::Attachment::Types::COLOR:
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

      case FBO::Attachment::Types::DEPTH:
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0,
                     GL_DEPTH_COMPONENT, GL_FLOAT, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, texture, 0);
        break;

      case FBO::Attachment::Types::STENCIL:
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

FrameBufferManager::~FrameBufferManager() {
  delete[] m_gl_fbos;
  delete[] m_gl_attachments;
}

void FrameBufferManager::destroy() {
  glDeleteFramebuffers(m_fbos.size(), m_gl_fbos);
  glDeleteTextures(m_attachments.size(), m_gl_attachments);
}

FBO FrameBufferManager::make(ObjectDef::ID object) {
  for (const auto& fbo : m_fbos) {
    if (fbo.object == object) {
      return fbo;
    }
  }
  throw std::runtime_error("Unable to find object");
}

void FrameBufferManager::bind_fbo(const FBO& fbo) const {
  auto i = 0;
  for (const auto& f : m_fbos) {
    if (f.object == fbo.object) {
      glBindFramebuffer(GL_FRAMEBUFFER, m_gl_fbos[i]);
      return;
    }
    i++;
  }
}

uint32_t FrameBufferManager::get_color_attachment(const FBO& fbo,
                                                  uint16_t index) const {
  auto seen = 0;
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == FBO::Attachment::Types::COLOR) {
      if (seen == index) {
        return m_gl_attachments[i];
      }
      seen++;
    }
  }
  return 0;
}

uint32_t FrameBufferManager::get_depth_attachment(const FBO& fbo) const {
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == FBO::Attachment::Types::DEPTH) {
      return m_gl_attachments[i];
    }
  }
  return 0;
}

uint32_t FrameBufferManager::get_stencil_attachment(const FBO& fbo) const {
  for (auto i = fbo.start; i < fbo.end; ++i) {
    auto& attachment = m_attachments[i];
    if (attachment.type == FBO::Attachment::Types::STENCIL) {
      return m_gl_attachments[i];
    }
  }
  return 0;
}

void FrameBufferManager::bind_color_attachment(const FBO& fbo,
                                               uint16_t index) const {
  glBindTexture(GL_TEXTURE_2D, get_color_attachment(fbo, index));
}

void FrameBufferManager::bind_depth_attachment(const FBO& fbo) const {
  glBindTexture(GL_TEXTURE_2D, get_depth_attachment(fbo));
}

void FrameBufferManager::bind_stencil_attachment(const FBO& fbo) const {
  glBindTexture(GL_TEXTURE_2D, get_stencil_attachment(fbo));
}
