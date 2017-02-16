#include "hans/context.hpp"

hans::context::context(hans::EngineData& data, hans::AudioBuses& buses)
    : settings(data.settings),
      strings(data.strings),
      registers(data.settings, data.registers),
      parameters(data.parameters.handles, data.parameters.buffer),
      shaders(strings, data.shaders),
      fbos(data.fbos, data.fbos_attachments),
      audio_buffers(data.audio_buffers),
      audio_buses(buses),
      ring_buffers(data.settings.blocksize, data.ring_buffers) {
}
