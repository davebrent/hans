#include "hans/jsonrpc/methods/GetParameters.hpp"

using namespace hans;

jsonrpc::GetParameters::GetParameters(
    const std::vector<hans_parameter>& parameters,
    const memory::StringManager& string_manager)
    : m_parameters(parameters), m_string_manager(string_manager) {
}

void jsonrpc::GetParameters::execute(const jsonrpc::Message& request,
                                     jsonrpc::Message& response) {
  auto parameters = nlohmann::json::array();

  for (auto& parameter : m_parameters) {
    auto values = nlohmann::json::array();
    auto param = nlohmann::json::object();

    if (parameter.values != nullptr) {
      for (int i = 0; i < parameter.size; ++i) {
        values[i] = parameter.values[i];
      }
    } else {
      for (int i = 0; i < parameter.size; ++i) {
        values[i] = 0;
      }
    }

    param["id"] = parameter.id;
    param["object_id"] = parameter.object_id;
    param["name"] = m_string_manager.lookup(parameter.name);
    param["size"] = parameter.size;
    param["values"] = values;
    parameters.push_back(param);
  }

  response.m_data["result"] = parameters;
}
