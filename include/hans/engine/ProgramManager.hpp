#ifndef HANS_ENGINE_PROGRAMMANAGER_H_
#define HANS_ENGINE_PROGRAMMANAGER_H_

#include "hans/common/types.hpp"
#include "hans/engine/Program.hpp"
#include "hans/engine/ProgramResources.hpp"
#include <vector>
#include <memory>

namespace hans {
namespace engine {

class ProgramManager {
 public:
  explicit ProgramManager(hans::engine::ProgramResources& resources);
  ~ProgramManager();

  /// Sets data on an existing program OR creates the program with name
  bool set(hans_hash name, common::ObjectGraph& graphics_graph,
           common::ObjectGraph& audio_graph);

  /// Specify a program to be used, only one program may be used at a time
  // XXX: Maybe a concept of audio bus's and multi-windows could be expanded
  //      so that more than one program may be active
  bool use(hans_hash name);

  void process_graphics();
  void process_audio();

  /// Returns true if name is the active program
  bool is_active(hans_hash name) const;

  /// Select no program, returns the index to the previously active program
  int release_active();

  /// Close all resources on all programs
  void destroy();

 private:
  hans::engine::ProgramResources& m_resources;
  std::vector<hans_hash> m_names;
  std::vector<std::unique_ptr<hans::engine::Program> > m_programs;
  int m_active_program;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PROGRAMMANAGER_H_
