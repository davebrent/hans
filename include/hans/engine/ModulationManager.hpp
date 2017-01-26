#ifndef HANS_ENGINE_MODULATIONMANAGER_H_
#define HANS_ENGINE_MODULATIONMANAGER_H_

#include <algorithm>
#include <atomic>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "hans/common/primitives.hpp"
#include "hans/engine/ParameterManager.hpp"

namespace hans {
namespace engine {
namespace modulation {

class DoubleBuffer {
 public:
  DoubleBuffer(size_t framesize);
  void write(const std::vector<Parameter::Value>& values);
  std::vector<Parameter::Value> load();
  void swap();

 private:
  bool m_swapped;
  std::vector<Parameter::Value> m_front;
  std::vector<Parameter::Value> m_back;
};

class RingBuffer {
 public:
  RingBuffer(size_t framesize, size_t frames);
  void write(const std::vector<Parameter::Value>& values);
  std::vector<Parameter::Value> load();
  size_t available();

 private:
  std::vector<Parameter::Value> m_buffer;
  size_t m_head;
  size_t m_tail;
  size_t m_framesize;
  size_t m_frames;
  size_t m_available;
};

class Worker {
 public:
  Worker(const Modulation::Thread& data, std::vector<Parameter::Value>& values,
         size_t start, size_t end);
  std::vector<Parameter::Value> modulate(
      const Modulation::Thread& index,
      const std::vector<Parameter::Value>& other);
  void restore();

 private:
  const Modulation::Thread& m_data;
  std::vector<Parameter::Value>& m_values;
  std::vector<Parameter::Value> m_snapshot;
  std::vector<Parameter::Value> m_output;
  size_t m_start;
  size_t m_end;
};

} // namespace modulation

class ModulationManager {
 public:
  ModulationManager(const ModulationManager& other) = delete;
  ModulationManager(const Modulation& modulation, Parameters& parameters);
  void snd_modulate();
  void snd_restore();
  void gfx_modulate();
  void gfx_restore();

 private:
  const Modulation& m_modulation;
  const Parameters& m_parameters;

  modulation::DoubleBuffer m_double_buffer;
  modulation::RingBuffer m_ring_buffer;

  modulation::Worker m_snd;
  modulation::Worker m_gfx;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_MODULATIONMANAGER_H_
