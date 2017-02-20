#include "hans/audio_buses.hpp"
#include <cstring>
#include <stdexcept>

using namespace hans;

AudioBuses::AudioBuses(const Settings::Audio& settings, size_t num)
    : _settings(settings) {
  auto frames = num * settings.input_channels.size() * settings.blocksize;
  auto bytes = frames * sizeof(audio::sample);

  _allocator.reset(bytes);
  auto data = _allocator.allocate(bytes, alignof(audio::sample));

  _base = static_cast<char*>(data);
  _ids = 0;
  _max = num;

  _revisions = new uint64_t[num];
}

AudioBuses::~AudioBuses() {
  delete[] _revisions;
}

audio::bus_handle AudioBuses::make() {
  if (_ids == _max) {
    throw std::runtime_error("AudioBuses: Exceeded number of buses");
  }

  auto handle = _ids;
  _ids++;
  return handle;
}

bool AudioBuses::write(audio::bus_handle handle, uint8_t channel,
                       const audio::sample* samples) {
  auto size = sizeof(audio::sample);
  auto offset =
      handle * _settings.blocksize * _settings.input_channels.size() * size;
  auto block = _base + offset + (_settings.blocksize * channel * size);
  _revisions[handle]++;
  std::memcpy(block, samples, size * _settings.blocksize);
  return true;
}

audio::sample* AudioBuses::read(audio::bus_handle handle, uint8_t channel) {
  auto size = sizeof(audio::sample);
  auto offset =
      handle * _settings.blocksize * _settings.input_channels.size() * size;
  auto block = _base + offset + (_settings.blocksize * channel * size);
  return reinterpret_cast<audio::sample*>(block);
}

bool AudioBuses::is_dirty(audio::bus_handle handle) {
  return _revisions[handle] != 0;
}

void AudioBuses::set_clean(audio::bus_handle handle) {
  _revisions[handle] = 0;
}
