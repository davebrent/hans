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
  ProgramManager(hans_object_api& api);
  /// Use a given set of data to work
  void use(common::ListView<hans_object>& objects,
           common::ListView<hans_program>& programs,
           common::ListView<size_t>& chains, hans_blob init_object_data);
  /// Initialize objects for all programs
  void setup_all();
  /// Destroys all objects for all programs
  void close_all();
  /// Switch to a different program
  void switch_to(hans_hash name);
  /// Tick the active programs graphics graph
  void tick_graphics(float delta);
  /// Tick the active programs audio graph
  void tick_audio();

 private:
  size_t* m_chains = nullptr;
  hans_object* m_objects = nullptr;
  hans_program* m_programs = nullptr;
  hans_object_api& m_api;
  size_t m_active = 0;
  size_t m_num_objects = 0;
  common::LinearAllocator m_allocator;
  hans_blob m_init_object_data;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PROGRAMMANAGER_H_
