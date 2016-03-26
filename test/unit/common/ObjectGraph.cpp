#include "hans/common/ObjectGraph.hpp"
#include <catch.hpp>

using namespace hans;

TEST_CASE("sorting an object graph", "[graph]") {
  SECTION("sorting topologicaly") {
    common::ObjectGraph graph(4, 4, 0);

    graph.object_at(0)->object_id = 10;
    graph.object_at(0)->instance_id = 0;
    graph.object_at(1)->object_id = 11;
    graph.object_at(1)->instance_id = 1;
    graph.object_at(2)->object_id = 12;
    graph.object_at(2)->instance_id = 2;
    graph.object_at(3)->object_id = 13;
    graph.object_at(3)->instance_id = 3;
    //      11(1)
    //     /     \
    //   10(0)  13(3)
    //     \     /
    //      12(2)
    graph.connection_at(0)->source = 1;
    graph.connection_at(0)->sink = 0;
    graph.connection_at(1)->source = 1;
    graph.connection_at(1)->sink = 3;
    graph.connection_at(2)->source = 0;
    graph.connection_at(2)->sink = 2;
    graph.connection_at(3)->source = 3;
    graph.connection_at(3)->sink = 2;

    REQUIRE(graph.topological_sort() == true);

    REQUIRE(graph.object_at(0)->object_id == 11);
    REQUIRE(graph.object_at(0)->instance_id == 1);

    REQUIRE(graph.object_at(1)->object_id == 13);
    REQUIRE(graph.object_at(1)->instance_id == 3);

    REQUIRE(graph.object_at(2)->object_id == 10);
    REQUIRE(graph.object_at(2)->instance_id == 0);

    REQUIRE(graph.object_at(3)->object_id == 12);
    REQUIRE(graph.object_at(3)->instance_id == 2);

    REQUIRE(graph.connection_at(0)->source == 0); // 11
    REQUIRE(graph.connection_at(0)->sink == 2);   // 10

    REQUIRE(graph.connection_at(1)->source == 0); // 11
    REQUIRE(graph.connection_at(1)->sink == 1);   // 13

    REQUIRE(graph.connection_at(2)->source == 2); // 10
    REQUIRE(graph.connection_at(2)->sink == 3);   // 12

    REQUIRE(graph.connection_at(3)->source == 1); // 13
    REQUIRE(graph.connection_at(3)->sink == 3);   // 12
  }

  SECTION("no sorting needed") {
    common::ObjectGraph graph(3, 2, 0);

    graph.object_at(0)->object_id = 0;
    graph.object_at(0)->instance_id = 0;
    graph.object_at(1)->object_id = 1;
    graph.object_at(1)->instance_id = 1;
    graph.object_at(2)->object_id = 2;
    graph.object_at(2)->instance_id = 2;
    // (0) -> (1) -> (2)
    graph.connection_at(0)->source = 0;
    graph.connection_at(0)->outlet = 1;
    graph.connection_at(0)->sink = 1;
    graph.connection_at(0)->inlet = 0;

    graph.connection_at(1)->source = 1;
    graph.connection_at(1)->outlet = 0;
    graph.connection_at(1)->sink = 2;
    graph.connection_at(1)->inlet = 0;

    REQUIRE(graph.topological_sort() == true);
    REQUIRE(graph.connection_at(0)->source == 0);
    REQUIRE(graph.connection_at(0)->outlet == 1);
    REQUIRE(graph.connection_at(0)->sink == 1);
    REQUIRE(graph.connection_at(0)->inlet == 0);

    REQUIRE(graph.connection_at(1)->source == 1);
    REQUIRE(graph.connection_at(1)->outlet == 0);
    REQUIRE(graph.connection_at(1)->sink == 2);
    REQUIRE(graph.connection_at(1)->inlet == 0);
  }

  SECTION("sorting edges") {
    common::ObjectGraph graph(4, 4, 0);

    graph.object_at(0)->object_id = 10;
    graph.object_at(0)->instance_id = 0;
    graph.object_at(1)->object_id = 11;
    graph.object_at(1)->instance_id = 1;
    graph.object_at(2)->object_id = 12;
    graph.object_at(2)->instance_id = 2;
    graph.object_at(3)->object_id = 13;
    graph.object_at(3)->instance_id = 3;

    graph.connection_at(0)->source = 1;
    graph.connection_at(0)->sink = 0;
    graph.connection_at(1)->source = 1;
    graph.connection_at(1)->sink = 3;
    graph.connection_at(2)->source = 0;
    graph.connection_at(2)->sink = 2;
    graph.connection_at(3)->source = 3;
    graph.connection_at(3)->sink = 2;

    graph.sort_edges();

    REQUIRE(graph.connection_at(0)->source == 0);
    REQUIRE(graph.connection_at(1)->source == 1);
    REQUIRE(graph.connection_at(2)->source == 1);
    REQUIRE(graph.connection_at(3)->source == 3);
  }
}

TEST_CASE("iterating through an object graph", "[graph]") {
  common::ObjectGraph graph(3, 2, 0);

  SECTION("iterating through objects") {
    int i = 0;
    for (const auto& object : graph.get_objects()) {
      i += 1;
    }
    REQUIRE(i == 3);
  }

  SECTION("iterating through objects and making changes") {
    for (auto& object : graph.get_objects()) {
      object.inlets = 100;
    }
    for (const auto& object : graph.get_objects()) {
      REQUIRE(object.inlets == 100);
    }
  }

  SECTION("object mutability") {
    graph.object_at(0)->inlets = 100;
    graph.object_at(1)->inlets = 100;
    graph.object_at(2)->inlets = 100;

    for (auto& object : graph.get_objects()) {
      REQUIRE(object.inlets == 100);
      object.inlets = 10;
    }

    for (const auto& object : graph.get_objects()) {
      REQUIRE(object.inlets == 10);
    }
  }
}
