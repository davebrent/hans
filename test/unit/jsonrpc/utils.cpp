#include "hans/common/types.hpp"
#include "hans/common/ObjectGraph.hpp"
#include "hans/memory/StringManager.hpp"
#include "hans/jsonrpc/methods/utils.hpp"
#include <catch.hpp>
#include <json.hpp>

using namespace hans;
using namespace nlohmann;

SCENARIO("Deserializing graph", "[rpc]") {
  GIVEN("a requested json graph") {
    memory::StringManager s(16);
    json requested = {{"connections", {}},
                      {"objects",
                       {{{"object_id", 2},
                         {"args", {{{"name", "num_inlets"}, {"value", 8}}}}}}}};

    WHEN("deserialized") {
      THEN("no errors should occur") {
        common::ObjectGraph graph(1, 0, 1);
        bool result = jsonrpc::deserialize_graph_json(graph, s, requested);
        requested.clear();

        REQUIRE(result == true);
        REQUIRE(graph.object_at(0)->object_id == 2);
        REQUIRE(graph.argument_at(0)->type == HANS_NUMBER);
        REQUIRE(graph.argument_at(0)->number == 8);
      }
    }
  }
}
