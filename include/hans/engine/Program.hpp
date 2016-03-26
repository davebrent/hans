#ifndef HANS_ENGINE_PROGRAM_H_
#define HANS_ENGINE_PROGRAM_H_

#include "hans/common/types.hpp"
#include "hans/common/ObjectGraph.hpp"
#include "hans/engine/object.hpp"
#include "hans/engine/ProgramResources.hpp"
#include "hans/engine/ObjectChain.hpp"

namespace hans {
namespace engine {

class Program {
 public:
  explicit Program(hans::engine::ProgramResources& resources);
  bool set(hans::common::ObjectGraph& audio,
           hans::common::ObjectGraph& graphics);
  void process_graphics();
  void process_audio();
  void destroy();

 private:
  hans::engine::ProgramResources& m_resources;
  hans::engine::ParameterManager m_parameter_manager;
  hans::engine::RegisterManager m_audio_register_manager;
  hans::engine::RegisterManager m_graphics_register_manager;

  hans::graphics::ShaderManager m_shader_manager;
  hans::graphics::FrameBufferManager m_frame_buffer_manager;
  hans_object_api m_audio_api;
  hans_object_api m_graphics_api;

  hans::engine::ObjectChain<hans_audio_object> m_audio_chain;
  hans::engine::ObjectChain<hans_graphics_object> m_graphics_chain;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PROGRAM_H_
