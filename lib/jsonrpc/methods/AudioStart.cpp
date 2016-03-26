#include "hans/jsonrpc/methods/AudioStart.hpp"

using json = nlohmann::json;
using namespace hans;

jsonrpc::AudioStart::AudioStart(audio::AudioStream& audio_stream)
    : m_audio_stream(audio_stream) {
}

void jsonrpc::AudioStart::execute(const jsonrpc::Message& request,
                                  jsonrpc::Message& response) {
  response.m_data["result"] = m_audio_stream.start();
}
