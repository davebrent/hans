#ifndef HANS_ENGINE_MODULATIONMANAGER_H_
#define HANS_ENGINE_MODULATIONMANAGER_H_

#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/ParameterManager.hpp"

namespace hans {
namespace engine {

class ModulationManager {
 public:
  ModulationManager(ParameterManager& parameter_manager,
                    const common::ListView<hans_modulator>& modulators);
  void setup();
  void begin();
  void end();

 private:
  const common::ListView<hans_modulator>& m_mods;
  ParameterManager& m_parameter_manager;
  hans_parameter* m_srcs;
  hans_parameter* m_dests;
  hans_parameter_value* m_vals;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_MODULATIONMANAGER_H_
