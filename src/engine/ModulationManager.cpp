#include "hans/engine/ModulationManager.hpp"

using namespace hans::common;
using namespace hans::engine;

ModulationManager::ModulationManager(ParameterManager& parameter_manager,
                                     const ListView<hans_modulator>& modulators)
    : m_parameter_manager(parameter_manager), m_mods(modulators) {
  auto num = m_mods.size();

  m_srcs = new hans_parameter[num];
  m_dests = new hans_parameter[num];
  m_vals = new hans_parameter_value[num];
}

void ModulationManager::setup() {
  for (auto i = 0; i < m_mods.size(); ++i) {
    const auto& m = m_mods[i];
    m_srcs[i] = m_parameter_manager.make(m.source.object, m.source.parameter);
    m_dests[i] = m_parameter_manager.make(m.dest.object, m.dest.parameter);
  }
}

void ModulationManager::begin() {
  // Store
  for (auto i = 0; i < m_mods.size(); ++i) {
    const auto& dest = m_dests[i];
    const auto& component = m_mods[i].dest.component;
    m_vals[i] = m_parameter_manager.get(dest, component);
  }

  // Modulate
  for (auto i = 0; i < m_mods.size(); ++i) {
    const auto& mod = m_mods[i];
    const auto& src = m_srcs[i];
    const auto& dest = m_dests[i];
    const auto& d_component = mod.dest.component;
    const auto& s_component = mod.source.component;

    const auto d_value = m_parameter_manager.get(dest, d_component);
    const auto s_value = m_parameter_manager.get(src, s_component);
    const auto value = d_value + ((s_value + mod.offset) * mod.scale);

    m_parameter_manager.set(dest, d_component, value);
  }
}

void ModulationManager::end() {
  // Restore
  for (auto i = 0; i < m_mods.size(); ++i) {
    const auto& dest = m_dests[i];
    const auto& component = m_mods[i].dest.component;
    auto value = m_vals[i];
    m_parameter_manager.set(dest, component, value);
  }
}