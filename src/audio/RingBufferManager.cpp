#include "hans/audio/RingBufferManager.hpp"
#include <cstring>
#include <stdexcept>

// TODO: Experiment with this value and its relation to blocksize
#define RB_FRAMES 30

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;

RingBufferManager::RingBufferManager(const Config& config,
                                     ListView<RingBuffer> ring_buffers)
    : m_ring_buffers(ring_buffers) {
  auto num_ring_buffers = m_ring_buffers.size();
  m_frame_size = sizeof(sample) * config.blocksize;

  auto single = m_frame_size * RB_FRAMES;
  auto bytes = num_ring_buffers * single;

  auto alignment = alignof(sample);
  m_allocator.reset(bytes);
  m_base = static_cast<char*>(m_allocator.allocate(bytes, alignment));

  m_available = new uint8_t[num_ring_buffers];
  m_heads = new uint8_t[num_ring_buffers];
  m_tails = new uint8_t[num_ring_buffers];

  auto offset = 0;
  auto index = 0;

  for (auto& ring_buffer : m_ring_buffers) {
    ring_buffer.offset = offset;
    ring_buffer.index = index;

    m_available[index] = 0;
    m_heads[index] = 0;
    m_tails[index] = 0;

    offset += single;
    index++;
  }
}

RingBuffer RingBufferManager::make(ObjectDef::ID producer, hash name) {
  for (const auto& ring : m_ring_buffers) {
    if (ring.producer == producer && ring.name == name) {
      return ring;
    }
  }
  throw std::runtime_error("RingBufferManager: Unknown ring buffer");
}

RingBuffer RingBufferManager::find(hash name) {
  for (const auto& ring : m_ring_buffers) {
    if (ring.name == name) {
      return ring;
    }
  }
  throw std::runtime_error("RingBufferManager: Unknown ring buffer");
}

bool RingBufferManager::write(RingBuffer ring, const sample* samples) {
  auto head = m_heads[ring.index];
  auto dest = m_base + ring.offset + (m_frame_size * head);

  std::memcpy(dest, samples, m_frame_size);

  m_heads[ring.index] = (head + 1) % RB_FRAMES;
  m_available[ring.index] += 1;
  return m_available[ring.index] < RB_FRAMES;
}

sample* RingBufferManager::read(hash name, uint8_t frame) {
  auto ring = find(name);
  auto tail = m_tails[ring.index];
  auto samples =
      m_base + ring.offset + (m_frame_size * ((tail + frame) % RB_FRAMES));
  return reinterpret_cast<sample*>(samples);
}

uint8_t RingBufferManager::available(hash name) {
  auto ring = find(name);
  return m_available[ring.index];
}

void RingBufferManager::advance_all() {
  for (auto i = 0; i < m_ring_buffers.size(); ++i) {
    auto available = m_available[i];
    m_tails[i] = (m_tails[i] + available) % RB_FRAMES;
    m_available[i] -= available;
  }
}
