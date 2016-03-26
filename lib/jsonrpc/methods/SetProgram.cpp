#include "hans/common/ObjectGraph.hpp"
#include "hans/jsonrpc/methods/SetProgram.hpp"
#include "hans/jsonrpc/methods/utils.hpp"

using namespace hans;

jsonrpc::SetProgram::SetProgram(memory::StringManager& string_manager,
                                engine::ProgramManager& program_manager)
    : m_string_manager(string_manager), m_program_manager(program_manager) {
}

void jsonrpc::SetProgram::execute(const jsonrpc::Message& request,
                                  jsonrpc::Message& response) {
  nlohmann::json params = request.m_data["params"];
  nlohmann::json name_json = params["name"];
  nlohmann::json graphics = params["graphics"];
  nlohmann::json audio = params["audio"];

  if (!name_json.is_string()) {
    response.set_invalid_params({"name"});
    return;
  }

  bool valid_graphics = graphics.is_object();
  bool valid_audio = audio.is_object();

  if (!valid_graphics && !valid_audio) {
    response.set_invalid_params({"graphics", "audio"});
    return;
  }

  if (!valid_graphics) {
    response.set_invalid_params({"graphics"});
    return;
  }

  if (!valid_audio) {
    response.set_invalid_params({"audio"});
    return;
  }

  auto graphics_objects = graphics["objects"];
  auto graphics_connections = graphics["connections"];

  if (!graphics_objects.is_array() || !graphics_connections.is_array()) {
    response.set_invalid_params({"graphics"});
    return;
  }

  auto audio_objects = audio["objects"];
  auto audio_connections = audio["connections"];

  if (!audio_objects.is_array() || !audio_connections.is_array()) {
    response.set_invalid_params({"audio"});
    return;
  }

  int num_graphics_args = 0;

  for (int i = 0; i < graphics_objects.size(); ++i) {
    nlohmann::json object = graphics_objects.at(i);

    if (!object.is_object()) {
      // TODO: Return a message specifying which index is not an object
      response.set_invalid_params({"graphics"});
      return;
    }

    nlohmann::json args = object["args"];
    if (args.is_array()) {
      num_graphics_args += args.size();
    }
  }

  int num_audio_args = 0;

  for (int i = 0; i < audio_objects.size(); ++i) {
    nlohmann::json object = audio_objects.at(i);

    if (!object.is_object()) {
      // TODO: Return a message specifying which index is not an object
      response.set_invalid_params({"audio"});
      return;
    }

    nlohmann::json args = object["args"];
    if (args.is_array()) {
      num_audio_args += args.size();
    }
  }

  common::ObjectGraph graphics_graph(
      graphics_objects.size(), graphics_connections.size(), num_graphics_args);

  if (!jsonrpc::deserialize_graph_json(graphics_graph, m_string_manager,
                                       graphics)) {
    response.set_runtime_error("Unable to deserialize graph", {"graphics"});
    return;
  }

  common::ObjectGraph audio_graph(audio_objects.size(),
                                  audio_connections.size(), num_audio_args);

  if (!jsonrpc::deserialize_graph_json(audio_graph, m_string_manager, audio)) {
    response.set_runtime_error("Unable to deserialize graph", {"audio"});
    return;
  }

  std::string name = name_json.get<std::string>();

  // Finally try and set the graph
  bool set_result = m_program_manager.set(m_string_manager.intern(name.c_str()),
                                          graphics_graph, audio_graph);

  if (!set_result) {
    response.set_runtime_error("Unable to create program (see log)", nullptr);
    return;
  }

  // Return the new graph (may have been altered by the manager.set call)
  nlohmann::json serialized_graphics_graph = nlohmann::json::object();
  nlohmann::json serialized_audio_graph = nlohmann::json::object();

  jsonrpc::serialize_graph_json(graphics_graph, serialized_graphics_graph);
  jsonrpc::serialize_graph_json(audio_graph, serialized_audio_graph);

  nlohmann::json result = nlohmann::json::object();
  result["name"] = name;
  result["graphics"] = serialized_graphics_graph;
  result["audio"] = serialized_audio_graph;
  response.m_data["result"] = result;
}
