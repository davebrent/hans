#include "hans/graphics/gl.h"
#include "hans/graphics/FrameBufferManager.hpp"
#include <cassert>

using namespace hans;

graphics::FrameBufferManager::FrameBufferManager(
    const std::vector<hans_fbo>& defaults)
    : m_frame_buffer_defaults(defaults) {
  m_frame_buffers = nullptr;
  m_attachments = nullptr;

  m_num_frame_buffers = 0;
  m_num_attachments = 0;

  m_offset_frame_buffers = 0;
  m_offset_attachments = 0;
}

graphics::FrameBufferManager::~FrameBufferManager() {
  if (m_frame_buffers != nullptr) {
    glDeleteFramebuffers(m_num_frame_buffers, m_frame_buffers);
  }

  if (m_attachments != nullptr) {
    glDeleteTextures(m_num_attachments, m_attachments);
  }

  delete[] m_frame_buffers;
  delete[] m_attachments;
}

uint16_t graphics::FrameBufferManager::set_objects(
    const std::vector<hans_object_id>& object_ids) {
  GLint max_color_attachments = 0;
  GLint max_draw_buffers = 0;

  glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, &max_color_attachments);
  glGetIntegerv(GL_MAX_DRAW_BUFFERS, &max_draw_buffers);

  for (auto& fbo : m_frame_buffer_defaults) {
    // XXX: Does this make sense now the depth buffer config sits inside this
    //      array?
    assert(fbo.num_attachments < max_color_attachments &&
           "Color attachment limit exceeded");
    assert(fbo.num_attachments < max_draw_buffers &&
           "Unexpected number of draw buffers");
  }

  uint16_t num_attachments = 0;
  uint16_t num_handles = 0;

  for (const auto& frame_buffer : m_frame_buffer_defaults) {
    for (const auto& id : object_ids) {
      if (frame_buffer.object_id == id) {
        num_attachments += frame_buffer.num_attachments;
        num_handles++;
      }
    }
  }

  set_capacity(num_handles, num_attachments);
  return num_handles;
}

void graphics::FrameBufferManager::set_capacity(uint16_t num_frame_buffers,
                                                uint16_t num_attachments) {
  m_num_frame_buffers = num_frame_buffers;
  m_num_attachments = num_attachments;

  delete[] m_frame_buffers;
  delete[] m_attachments;

  m_frame_buffers = new uint32_t[m_num_frame_buffers];
  m_attachments = new uint32_t[m_num_attachments];

  glGenFramebuffers(m_num_frame_buffers, m_frame_buffers);
  glGenTextures(m_num_attachments, m_attachments);

  m_offset_frame_buffers = 0;
  m_offset_attachments = 0;
}

int graphics::FrameBufferManager::make(hans_object_resource* resources,
                                       const hans_object_id object_id,
                                       const hans_instance_id instance_id) {
  static const GLenum color_attachments[8] = {
      GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2,
      GL_COLOR_ATTACHMENT3, GL_COLOR_ATTACHMENT4, GL_COLOR_ATTACHMENT5,
      GL_COLOR_ATTACHMENT6, GL_COLOR_ATTACHMENT7};

  int created = 0;
  for (const auto& frame_buffer : m_frame_buffer_defaults) {
    if (frame_buffer.object_id != object_id) {
      continue;
    }

    resources->type = HANS_FRAME_BUFFER;
    resources->frame_buffer.fbo_index = m_offset_frame_buffers;
    resources->frame_buffer.attachments_index = m_offset_attachments;

    // Setup all color attachments for this frame buffer
    // Attachments are ordered in the texture array by:
    // [color0, color1, color2, .., depth, stencil]
    glBindFramebuffer(GL_FRAMEBUFFER, m_frame_buffers[m_offset_frame_buffers]);

    // Make all the color attachments
    int num_color_attachments = 0;
    for (int i = 0; i < frame_buffer.num_attachments; ++i) {
      auto conf = frame_buffer.attachments[i];
      if (conf.type == HANS_COLOR_ATTACHMENT) {
        glBindTexture(GL_TEXTURE_2D, m_attachments[m_offset_attachments]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, conf.width, conf.height, 0,
                     GL_RGBA, GL_UNSIGNED_BYTE, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER,
                             GL_COLOR_ATTACHMENT0 + num_color_attachments,
                             m_attachments[m_offset_attachments], 0);

        num_color_attachments++;
        m_offset_attachments++;
      }
    }

    resources->frame_buffer.num_color_attachments = num_color_attachments;

    // Make all the depth attachment if its requested
    for (int i = 0; i < frame_buffer.num_attachments; ++i) {
      auto conf = frame_buffer.attachments[i];
      if (conf.type == HANS_DEPTH_ATTACHMENT) {
        glBindTexture(GL_TEXTURE_2D, m_attachments[m_offset_attachments]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, conf.width,
                     conf.height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
                             m_attachments[m_offset_attachments], 0);

        resources->frame_buffer.has_depth = true;
        m_offset_attachments++;
        break;
      }
    }

    // Make all the depth attachment if its requested
    for (int i = 0; i < frame_buffer.num_attachments; ++i) {
      auto conf = frame_buffer.attachments[i];
      if (conf.type == HANS_STENCIL_ATTACHMENT) {
        glBindTexture(GL_TEXTURE_2D, m_attachments[m_offset_attachments]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, conf.width, conf.height, 0,
                     GL_RGBA, GL_UNSIGNED_BYTE, 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glFramebufferTexture(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT,
                             m_attachments[m_offset_attachments], 0);

        resources->frame_buffer.has_stencil = true;
        m_offset_attachments++;
        break;
      }
    }

    // Defines the array of buffers into which outputs from the fragment shader
    // data will be written to
    glDrawBuffers(num_color_attachments, color_attachments);
    // Check that the FBO is complete
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE) {
      created++;
    } else {
      // TODO: Raise an error
    }

    m_offset_frame_buffers++;
    resources++;
  }

  return created;
}

void graphics::FrameBufferManager::release_frame_buffer() const {
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void graphics::FrameBufferManager::bind_frame_buffer(
    const hans_fbo_handle& handle) const {
  glBindFramebuffer(GL_FRAMEBUFFER, m_frame_buffers[handle.fbo_index]);
}

uint32_t graphics::FrameBufferManager::get_color_attachment(
    const hans_fbo_handle& handle, uint16_t index) const {
  return m_attachments[handle.attachments_index + index];
}

uint32_t graphics::FrameBufferManager::get_depth_attachment(
    const hans_fbo_handle& handle) const {
  if (handle.has_depth) {
    auto index = handle.attachments_index + handle.num_color_attachments;
    return m_attachments[index];
  }
  return 0;
}

uint32_t graphics::FrameBufferManager::get_stencil_attachment(
    const hans_fbo_handle& handle) const {
  if (handle.has_stencil) {
    auto index = handle.attachments_index + handle.num_color_attachments;
    if (handle.has_depth) {
      return m_attachments[index + 1];
    }
    return m_attachments[index];
  }
  return 0;
}

void graphics::FrameBufferManager::bind_color_attachment(
    const hans_fbo_handle& handle, uint16_t index) const {
  glBindTexture(GL_TEXTURE_2D, get_color_attachment(handle, index));
}

void graphics::FrameBufferManager::bind_depth_attachment(
    const hans_fbo_handle& handle) const {
  if (handle.has_depth) {
    glBindTexture(GL_TEXTURE_2D, get_depth_attachment(handle));
  }
}

void graphics::FrameBufferManager::bind_stencil_attachment(
    const hans_fbo_handle& handle) const {
  if (handle.has_stencil) {
    glBindTexture(GL_TEXTURE_2D, get_stencil_attachment(handle));
  }
}
