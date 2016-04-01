#include "hans/jsonrpc/methods/GetShaders.hpp"

using namespace hans;

jsonrpc::GetShaders::GetShaders(const memory::StringManager& string_manager,
                                const std::vector<hans_shader>& shaders)
    : m_string_manager(string_manager), m_shaders(shaders) {
}

void jsonrpc::GetShaders::execute(const jsonrpc::Message& request,
                                  jsonrpc::Message& response) {
  auto result = nlohmann::json::array();

  for (auto& shader : m_shaders) {
    auto obj = nlohmann::json::object();
    obj["uri"] = m_string_manager.lookup(shader.uri);
    obj["code"] = m_string_manager.lookup(shader.code);
    result.push_back(obj);
  }

  response.m_data["result"] = result;
}
