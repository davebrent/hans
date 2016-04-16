#ifndef HANS_JSONRPC_METHODS_GETAUDIODEVICES_H_
#define HANS_JSONRPC_METHODS_GETAUDIODEVICES_H_

#include "hans/audio/AudioDevices.hpp"
#include "hans/common/types.hpp"
#include "hans/jsonrpc/Method.hpp"

namespace hans {
namespace jsonrpc {

/// A command for listing available objects
class GetAudioDevices : public virtual hans::jsonrpc::Method {
 public:
  // FIXME: Should be marked const
  explicit GetAudioDevices(hans::audio::AudioDevices& audio_devices);
  void execute(const hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  hans::audio::AudioDevices& m_audio_devices;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_GETAUDIODEVICES_H_
