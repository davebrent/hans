#include "hans/jsonrpc/methods/utils.hpp"

using namespace nlohmann;
using namespace hans;

bool jsonrpc::deserialize_graph_json(common::ObjectGraph& graph,
                                     memory::StringManager& string_manager,
                                     const json& request) {
  json nodes = request["objects"];
  json edges = request["connections"];

  int graph_arg_counter = 0;

  for (int i = 0; i < nodes.size(); ++i) {
    json& object = nodes.at(i);
    json& id = object["object_id"];

    if (!id.is_number()) {
      return false;
    }

    hans_user_object* user_object = graph.object_at(i);
    user_object->object_id = id;
    user_object->arguments = nullptr;
    user_object->arguments_len = 0;
    user_object->arguments_index = 0;
    user_object->inlets = 0;
    user_object->outlets = 0;

    json& arguments = object["args"];
    if (!arguments.is_array()) {
      continue;
    }

    user_object->arguments = graph.argument_at(graph_arg_counter);
    user_object->arguments_index = graph_arg_counter;
    user_object->arguments_len = arguments.size();

    for (auto& argument : arguments) {
      json& name = argument["name"];
      json& value = argument["value"];

      if (!name.is_string()) {
        return false;
      }

      auto arg = graph.argument_at(graph_arg_counter);
      arg->name = string_manager.intern(name.get<std::string>().c_str());

      if (value.is_string()) {
        arg->type = HANS_STRING;
        arg->string = string_manager.intern(value.get<std::string>().c_str());
      } else if (value.is_number()) {
        arg->type = HANS_NUMBER;
        arg->number = value.get<int>();
      } else if (value.is_boolean()) {
        arg->type = HANS_BOOL;
        arg->boolean = value.get<bool>();
      } else {
        return false;
      }

      graph_arg_counter++;
    }
  }

  for (int i = 0; i < edges.size(); ++i) {
    json& edge = edges.at(i);

    auto source = edge["source"];
    auto outlet = edge["outlet"];
    auto sink = edge["sink"];
    auto inlet = edge["inlet"];

    if (!source.is_number() || !outlet.is_number() || !sink.is_number() ||
        !inlet.is_number()) {
      return false;
    }

    auto connection = graph.connection_at(i);
    connection->source = source.get<unsigned int>();
    connection->outlet = outlet.get<unsigned int>();
    connection->sink = sink.get<unsigned int>();
    connection->inlet = inlet.get<unsigned int>();
  }

  return true;
}

bool jsonrpc::serialize_graph_json(const common::ObjectGraph& graph,
                                   json& result) {
  json json_objects = json::array();
  json json_connections = json::array();

  for (auto& object : graph.get_objects()) {
    json user_object = json::object();
    user_object["object_id"] = object.object_id;
    user_object["instance_id"] = object.instance_id;
    user_object["inlets"] = object.inlets;
    user_object["outlets"] = object.outlets;
    json_objects.push_back(user_object);
  }

  for (auto& connection : graph.get_connections()) {
    json edge = json::object();
    edge["source"] = connection.source;
    edge["outlet"] = connection.outlet;
    edge["sink"] = connection.sink;
    edge["inlet"] = connection.inlet;
    json_connections.push_back(edge);
  }

  result["objects"] = json_objects;
  result["connections"] = json_connections;
  return true;
}
