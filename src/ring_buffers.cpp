#include "hans/ring_buffers.hpp"
#include <cstring>
#include <iostream>
#include <stdexcept>

// TODO: Experiment with this value and its relation to blocksize
#define RB_FRAMES 30

using namespace hans;

RingBufferManager::RingBufferManager(uint16_t blocksize,
                                     std::vector<RingBuffer>& ring_buffers)
    : m_ring_buffers(ring_buffers) {
  auto num_ring_buffers = m_ring_buffers.size();
  m_frame_size = sizeof(audio::sample) * blocksize;

  auto single = m_frame_size * RB_FRAMES;
  auto bytes = num_ring_buffers * single;

  auto alignment = alignof(audio::sample);
  m_allocator.reset(bytes);
  m_base = static_cast<char*>(m_allocator.allocate(bytes, alignment));

  m_available = new uint8_t[num_ring_buffers]();
  m_heads = new uint8_t[num_ring_buffers]();
  m_tails = new uint8_t[num_ring_buffers]();
}

RingBufferManager::~RingBufferManager() {
  delete[] m_available;
  delete[] m_heads;
  delete[] m_tails;
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

bool RingBufferManager::write(const RingBuffer& ring,
                              const audio::sample* samples) {
  auto offset = m_frame_size * RB_FRAMES * ring.index;
  auto head = m_heads[ring.index];
  auto dest = m_base + offset + (m_frame_size * head);

  std::memcpy(dest, samples, m_frame_size);

  m_heads[ring.index] = (head + 1) % RB_FRAMES;
  m_available[ring.index] += 1;
  return m_available[ring.index] < RB_FRAMES;
}

audio::sample* RingBufferManager::read(hash name, uint8_t frame) {
  auto ring = find(name);
  auto offset = m_frame_size * RB_FRAMES * ring.index;
  auto tail = m_tails[ring.index];
  auto samples =
      m_base + offset + (m_frame_size * ((tail + frame) % RB_FRAMES));
  return reinterpret_cast<audio::sample*>(samples);
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
