#include "hans/audio/AudioBufferManager.hpp"
#include <cstdlib>
#include <stdexcept>

using namespace hans;

audio::AudioBufferManager::AudioBufferManager(uint16_t blocksize)
    : m_blocksize(blocksize) {
}

audio::AudioBufferManager::~AudioBufferManager() {
  for (void* buff : m_buffers) {
    std::free(buff);
  }

  m_buffers.clear();
}

int audio::AudioBufferManager::make(hans_object_resource* resources, int len) {
  for (int a = 0; a < len; ++a) {
    resources->type = HANS_AUDIO_BUFFER;
    resources->audio_buffer = create(1, m_blocksize, 1);
    resources++;
  }
  return len;
}

hans_audio_buffer* audio::AudioBufferManager::create(uint8_t num_channels,
                                                     unsigned num_samples,
                                                     unsigned num_buffers) {
  auto ptr_chan_arr = sizeof(hans_audio_sample*) * num_channels;
  auto all_ptr_chan_arr = ptr_chan_arr * num_buffers;

  auto obj = sizeof(hans_audio_buffer);
  auto all_obj = obj * num_buffers;

  auto samps = sizeof(hans_audio_sample) * num_samples;
  auto all_samps = samps * num_buffers * num_channels;

  void* raw = std::calloc(1, all_obj + all_ptr_chan_arr + all_samps);

  if (raw == nullptr) {
    throw std::runtime_error("Out of memory");
  }

  m_buffers.push_back(raw);

  char* base = static_cast<char*>(raw);
  char* obj_ptr = base;
  char* samps_ptr = base + all_obj;
  char* buff_ptr = base + all_obj + all_ptr_chan_arr;

  // | c | f | c** | c | f | c** | c* | c* | c* | .. | c | c | c | ..
  //               |             |                   |

  for (int i = 0; i < num_buffers; ++i) {
    hans_audio_buffer* au_buffer =
        reinterpret_cast<hans_audio_buffer*>(obj_ptr);
    au_buffer->channels = num_channels;
    au_buffer->samples_len = num_samples;
    obj_ptr += obj;

    au_buffer->samples = reinterpret_cast<hans_audio_sample**>(samps_ptr);
    samps_ptr += ptr_chan_arr;

    for (int c = 0; c < num_channels; ++c) {
      au_buffer->samples[c] = reinterpret_cast<hans_audio_sample*>(buff_ptr);
      buff_ptr += samps;
    }
  }

  return reinterpret_cast<hans_audio_buffer*>(raw);
}
