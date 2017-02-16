#include "hans/modulation.hpp"

using namespace hans;
using namespace hans::modulation;

DoubleBuffer::DoubleBuffer(size_t framesize)
    : m_swapped(false), m_front(framesize), m_back(framesize) {
}

void DoubleBuffer::write(const std::vector<Parameter::Value>& values) {
  auto it = m_swapped ? m_front.begin() : m_back.begin();
  std::copy(values.begin(), values.end(), it);
}

std::vector<Parameter::Value> DoubleBuffer::load() {
  return m_swapped ? m_back : m_front;
}

void DoubleBuffer::swap() {
  m_swapped = !m_swapped;
}

modulation::RingBuffer::RingBuffer(size_t framesize, size_t frames)
    : m_buffer(framesize * frames),
      m_head(0),
      m_tail(0),
      m_framesize(framesize),
      m_frames(frames),
      m_available(0) {
}

void modulation::RingBuffer::write(
    const std::vector<Parameter::Value>& values) {
  auto offset = m_head * m_framesize;
  std::copy(values.begin(), values.end(), m_buffer.begin() + offset);
  m_head = (m_head + 1) % m_frames;
  m_available++;
}

size_t modulation::RingBuffer::available() {
  return m_available;
}

std::vector<Parameter::Value> modulation::RingBuffer::load() {
  auto offset = m_tail * m_framesize;
  auto result = std::vector<Parameter::Value>(&m_buffer[offset],
                                              &m_buffer[offset + m_framesize]);
  m_tail = (m_tail + 1) % m_frames;
  m_available--;
  return result;
}

Worker::Worker(const Modulation::Thread& data,
               std::vector<Parameter::Value>& values, size_t start, size_t end)
    : m_values(values),
      m_data(data),
      m_snapshot(end - start, 0),
      m_output(data.cross.size(), 0),
      m_start(start),
      m_end(end) {
}

std::vector<Parameter::Value> Worker::modulate(
    const Modulation::Thread& index,
    const std::vector<Parameter::Value>& other) {
  // Copy only "this" threads parameter buffer
  std::copy(m_values.begin() + m_start, m_values.begin() + m_end,
            m_snapshot.begin());

  // Other thread may not have been modulated yet
  if (index.cross.size() == other.size()) {
    // Merge other threads modulated parameters into this threads buffer
    for (auto m = 0; m < index.cross.size(); ++m) {
      m_values[index.cross[m].destination] += other[m];
    }
  }

  // Modulate this threads parameters in place
  for (auto i = 0; i < m_data.local.size(); ++i) {
    auto& modulator = m_data.local[i];
    auto source = m_values[modulator.source];
    auto dest = m_values[modulator.destination];
    auto value = dest + ((source + modulator.offset) * modulator.scale);
    m_values[modulator.destination] = value;
  }

  // Return cross modulated parameters, to be merged by other thread
  for (auto i = 0; i < m_data.cross.size(); ++i) {
    auto& modulator = m_data.cross[i];
    auto source = m_values[modulator.source];
    m_output[i] = (source + modulator.offset) * modulator.scale;
  }

  return m_output;
}

void Worker::restore() {
  std::copy(m_snapshot.begin(), m_snapshot.end(), m_values.begin() + m_start);
}

ModulationManager::ModulationManager(const Modulation& modulation,
                                     Parameters& parameters)
    : m_modulation(modulation),
      m_parameters(parameters),
      m_double_buffer(modulation.graphics.cross.size()),
      m_ring_buffer(modulation.audio.cross.size(), 100),
      m_snd(modulation.audio, parameters.buffer, 0, parameters.split),
      m_gfx(modulation.graphics, parameters.buffer, parameters.split,
            parameters.buffer.size()) {
}

void ModulationManager::snd_modulate() {
  auto gfx = m_double_buffer.load();
  auto snd = m_snd.modulate(m_modulation.graphics, gfx);
  m_ring_buffer.write(snd);
}

void ModulationManager::snd_restore() {
  m_snd.restore();
}

void ModulationManager::gfx_modulate() {
  auto length = m_modulation.audio.cross.size();
  std::vector<Parameter::Value> snd(length, 0);

  auto available = m_ring_buffer.available();

  if (available != 0) {
    for (auto i = 0; i < available; ++i) {
      auto data = m_ring_buffer.load();
      for (auto p = 0; p < length; ++p) {
        snd[p] += data[p];
      }
    }

    for (auto p = 0; p < length; ++p) {
      auto t = snd[p];
      snd[p] /= (Parameter::Value)available;
    }
  }

  auto gfx = m_gfx.modulate(m_modulation.audio, snd);
  m_double_buffer.write(gfx);
  m_double_buffer.swap();
}

void ModulationManager::gfx_restore() {
  m_gfx.restore();
}
