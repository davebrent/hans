#include "hans/jsonrpc/methods/AudioStop.hpp"

using json = nlohmann::json;
using namespace hans;

jsonrpc::AudioStop::AudioStop(audio::AudioStream& audio_stream)
    : m_audio_stream(audio_stream) {
}

void jsonrpc::AudioStop::execute(const jsonrpc::Message& request,
                                 jsonrpc::Message& response) {
  response.m_data["result"] = m_audio_stream.stop();
}
