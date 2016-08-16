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
                    const common::ListView<Modulator> modulators);
  void setup();
  void begin();
  void end();

 private:
  const common::ListView<Modulator> m_mods;
  ParameterManager& m_parameter_manager;
  Parameter* m_srcs;
  Parameter* m_dests;
  Parameter::Value* m_vals;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_MODULATIONMANAGER_H_
