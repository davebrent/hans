#ifndef HANS_JSONRPC_METHODS_AUDIOSTART_H_
#define HANS_JSONRPC_METHODS_AUDIOSTART_H_

#include "hans/audio/AudioStream.hpp"
#include "hans/jsonrpc/Method.hpp"

namespace hans {
namespace jsonrpc {

class AudioStart : public virtual hans::jsonrpc::Method {
 public:
  explicit AudioStart(hans::audio::AudioStream& audio_stream);
  void execute(const hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  hans::audio::AudioStream& m_audio_stream;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_AUDIOSTART_H_
