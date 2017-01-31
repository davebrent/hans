#include "hans/engine/context.hpp"

hans::engine::context::context(hans::EngineData& data)
    : settings(data.settings),
      strings(data.strings),
      registers(data.settings, data.registers),
      parameters(data.parameters.handles, data.parameters.buffer),
      shaders(strings, data.shaders),
      fbos(data.fbos, data.fbos_attachments),
      audio_buffers(data.audio_buffers),
      audio_buses(data.settings, 1),
      ring_buffers(data.settings.blocksize, data.ring_buffers) {
}
