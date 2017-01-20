#ifndef HANS_ENGINE_MODULATIONMANAGER_H_
#define HANS_ENGINE_MODULATIONMANAGER_H_

#include <vector>
#include "hans/common/primitives.hpp"
#include "hans/engine/ParameterManager.hpp"

namespace hans {
namespace engine {

class ModulationManager {
 public:
  ModulationManager(const ModulationManager& other) = delete;
  ModulationManager(ParameterManager& parameter_manager,
                    const std::vector<Modulator>& modulators);
  ~ModulationManager();
  void begin();
  void end();

 private:
  const std::vector<Modulator>& m_mods;
  ParameterManager& m_parameter_manager;
  Parameter* m_srcs;
  Parameter* m_dests;
  Parameter::Value* m_vals;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_MODULATIONMANAGER_H_
