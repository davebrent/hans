#ifndef HANS_JSONRPC_METHODS_UTILS_H_
#define HANS_JSONRPC_METHODS_UTILS_H_

#include "hans/common/types.hpp"
#include "hans/common/ObjectGraph.hpp"
#include "hans/memory/StringManager.hpp"
#include <json.hpp>

namespace hans {
namespace jsonrpc {

/// Transform a json graph representation to C struct
bool deserialize_graph_json(hans::common::ObjectGraph& graph,
                            hans::memory::StringManager& string_manager,
                            const nlohmann::json& request);

/// Transform a C struct graph to json
bool serialize_graph_json(const hans::common::ObjectGraph& graph,
                          nlohmann::json& request);

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_UTILS_H_
