#include "hans/engine/ProgramManager.hpp"
#include <algorithm>
#include <sstream>

using namespace hans;

engine::ProgramManager::ProgramManager(engine::ProgramResources& resources)
    : m_resources(resources) {
  m_active_program = -1;
}

engine::ProgramManager::~ProgramManager() {
}

bool engine::ProgramManager::set(hans_hash name,
                                 common::ObjectGraph& graphics_graph,
                                 common::ObjectGraph& audio_graph) {
  auto begin = m_names.begin();
  auto end = m_names.end();
  auto it = std::find(begin, end, name);

  if (it != end) {
    auto index = it - begin;
    std::ostringstream ss;
    ss << "PROGRAM removing=" << name << " index=" << index;
    m_resources.logger->log(common::Logger::DEBUG, ss.str().c_str());
    m_names.erase(m_names.begin() + index);
    m_programs.erase(m_programs.begin() + index);
  }

  auto program = std::make_unique<engine::Program>(m_resources);
  if (!program->set(audio_graph, graphics_graph)) {
    return false;
  }

  std::ostringstream ss;
  ss << "PROGRAM created=" << name;
  m_resources.logger->log(common::Logger::DEBUG, ss.str().c_str());
  m_programs.push_back(std::move(program));
  m_names.push_back(name);
  return true;
}

bool engine::ProgramManager::use(hans_hash name) {
  auto start = m_names.begin();
  auto end = m_names.end();
  auto it = std::find(start, end, name);

  if (it == end) {
    std::ostringstream ss;
    ss << "PROGRAM using=" << name << " - not found";
    m_resources.logger->log(common::Logger::ERROR, ss.str().c_str());
    return false;
  }

  m_active_program = it - start;
  std::ostringstream ss;
  ss << "PROGRAM using=" << name << " id=" << m_active_program;
  m_resources.logger->log(common::Logger::DEBUG, ss.str().c_str());
  return true;
}

bool engine::ProgramManager::is_active(hans_hash name) const {
  if (m_active_program < 0) {
    return false;
  }

  return name == m_names.at(m_active_program);
}

int engine::ProgramManager::release_active() {
  // XXX: Should these checks and setting of the active program be atomic? Thus
  //      side stepping threading issues between the audio and main thread?
  int temp = m_active_program;
  m_active_program = -1;
  return temp;
}

void engine::ProgramManager::process_graphics() {
  if (m_active_program >= 0 && m_active_program < m_programs.size()) {
    m_programs.at(m_active_program)->process_graphics();
  }
}

void engine::ProgramManager::process_audio() {
  if (m_active_program >= 0 && m_active_program < m_programs.size()) {
    m_programs.at(m_active_program)->process_audio();
  }
}

void engine::ProgramManager::destroy() {
  release_active();

  for (auto& program : m_programs) {
    program->destroy();
  }

  m_programs.clear();
}
