#include "hans/jsonrpc/methods/GetObjects.hpp"

using namespace hans;

jsonrpc::GetObjects::GetObjects(const memory::StringManager& string_manager,
                                const std::vector<hans_object>& objects)
    : m_string_manager(string_manager), m_objects(objects) {
}

void jsonrpc::GetObjects::execute(const jsonrpc::Message& request,
                                  jsonrpc::Message& response) {
  auto result = nlohmann::json::array();

  for (auto& object : m_objects) {
    auto data = nlohmann::json::object();
    data["id"] = object.id;
    data["name"] = m_string_manager.lookup(object.name);

    switch (object.type) {
    case HANS_AUDIO:
      data["type"] = "audio";
      break;
    case HANS_GRAPHICS:
      data["type"] = "graphics";
      break;
    }

    result.push_back(data);
  }

  response.m_data["result"] = result;
}
