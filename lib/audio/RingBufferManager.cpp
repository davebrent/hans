#include "hans/audio/RingBufferManager.hpp"
#include <cstring>
#include <stdexcept>

// TODO: Experiment with this value and its relation to blocksize
#define RB_FRAMES 30

using namespace hans;
using namespace hans::audio;

RingBufferManager::RingBufferManager(
    const hans_config& config,
    hans::common::ListView<hans_ring_buffer>& ring_buffers) {
  m_ring_buffers = &ring_buffers[0];
  m_ring_buffers_len = ring_buffers.size();

  m_frame_size = sizeof(hans_audio_sample) * config.blocksize;

  auto single = m_frame_size * RB_FRAMES;
  auto bytes = m_ring_buffers_len * single;

  auto alignment = alignof(hans_audio_sample);
  m_allocator.reset(bytes);
  m_base = static_cast<char*>(m_allocator.allocate(bytes, alignment));

  m_available = new uint8_t[m_ring_buffers_len];
  m_heads = new uint8_t[m_ring_buffers_len];
  m_tails = new uint8_t[m_ring_buffers_len];

  auto offset = 0;
  auto index = 0;

  for (auto& ring_buffer : ring_buffers) {
    ring_buffer.offset = offset;
    ring_buffer.index = index;

    m_available[index] = 0;
    m_heads[index] = 0;
    m_tails[index] = 0;

    offset += single;
    index++;
  }
}

hans_ring_buffer RingBufferManager::make(hans_instance_id producer,
                                         hans_hash name) {
  for (auto i = 0; i < m_ring_buffers_len; ++i) {
    auto& ring = m_ring_buffers[i];
    if (ring.producer == producer && ring.name == name) {
      return ring;
    }
  }
  throw std::runtime_error("RingBufferManager: Unknown ring buffer");
}

hans_ring_buffer RingBufferManager::find(hans_hash name) {
  for (auto i = 0; i < m_ring_buffers_len; ++i) {
    auto& ring = m_ring_buffers[i];
    if (ring.name == name) {
      return ring;
    }
  }
  throw std::runtime_error("RingBufferManager: Unknown ring buffer");
}

bool RingBufferManager::write(hans_ring_buffer ring,
                              const hans_audio_sample* samples) {
  auto head = m_heads[ring.index];
  auto dest = m_base + ring.offset + (m_frame_size * head);

  std::memcpy(dest, samples, m_frame_size);

  m_heads[ring.index] = (head + 1) % RB_FRAMES;
  m_available[ring.index] += 1;
  return m_available[ring.index] < RB_FRAMES;
}

hans_audio_sample* RingBufferManager::read(hans_hash name, uint8_t frame) {
  auto ring = find(name);
  auto tail = m_tails[ring.index];
  auto samples = m_base + (m_frame_size * ((tail + frame) % RB_FRAMES));
  return reinterpret_cast<hans_audio_sample*>(samples);
}

uint8_t RingBufferManager::available(hans_hash name) {
  auto ring = find(name);
  return m_available[ring.index];
}

void RingBufferManager::advance_all() {
  for (auto i = 0; i < m_ring_buffers_len; ++i) {
    auto available = m_available[i];
    m_tails[i] = (m_tails[i] + available) % RB_FRAMES;
    m_available[i] -= available;
  }
}
