#include "hans/jsonrpc/methods/GetAudioDevices.hpp"

using namespace hans;

jsonrpc::GetAudioDevices::GetAudioDevices(audio::AudioDevices& audio_devices)
    : m_audio_devices(audio_devices) {
}

void jsonrpc::GetAudioDevices::execute(const jsonrpc::Message& request,
                                       jsonrpc::Message& response) {
  auto result = nlohmann::json::array();

  for (auto& device : m_audio_devices) {
    auto data = nlohmann::json::object();
    data["id"] = device.id;
    data["name"] = device.name;
    data["max_input_channels"] = device.max_input_channels;
    data["max_output_channels"] = device.max_output_channels;
    data["default_input"] = device.default_input;
    data["default_output"] = device.default_output;
    result.push_back(data);
  }

  response.m_data["result"] = result;
}
