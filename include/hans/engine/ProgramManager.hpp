#ifndef HANS_ENGINE_PROGRAMMANAGER_H_
#define HANS_ENGINE_PROGRAMMANAGER_H_

#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/object.hpp"

namespace hans {
namespace engine {

class ProgramManager {
 public:
  ProgramManager(Engine& engine);
  /// Use a given set of data to work
  void use(common::ListView<ObjectDef> objects,
           common::ListView<Program> programs, common::ListView<size_t> chains,
           common::DataFile::Blob state);
  /// Initialize objects for all programs
  void setup_all();
  /// Destroys all objects for all programs
  void close_all();
  /// Switch to a different program
  void switch_to(hash name);
  /// Tick the active programs graphics graph
  void tick_graphics(float delta);
  /// Tick the active programs audio graph
  void tick_audio();

 private:
  Engine& m_engine;
  common::ListView<ObjectDef> m_objects;
  common::ListView<Program> m_programs;
  common::ListView<size_t> m_chains;
  common::DataFile::Blob m_default_state;
  size_t m_active = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PROGRAMMANAGER_H_
