#include "hans/memory/StringManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include <catch.hpp>
#include <memory>

using namespace hans;

TEST_CASE("parameter manager", "[parameters]") {
  memory::StringManager s(16);
  // object_id, id, name, size
  std::vector<hans_parameter> parameters = {{0, 0, s.intern("a"), 1},
                                            {1, 1, s.intern("b"), 2},
                                            {1, 2, s.intern("c"), 3},
                                            {2, 3, s.intern("d"), 1}};

  SECTION("parameters for existing objects") {
    engine::ParameterManager parameter_manager(parameters);
    parameter_manager.set_capacity(5);

    auto resources = std::make_unique<hans_object_resource[]>(2);
    auto created = parameter_manager.make(resources.get(), 1, 0);

    REQUIRE(resources[0].name == s.intern("b"));
    REQUIRE(resources[1].name == s.intern("c"));
    REQUIRE(created == 2);

    SECTION("setting and retrieving parameter values") {
      parameter_manager.set(resources[0].parameter, 0, 100);
      REQUIRE(parameter_manager.get(resources[0].parameter, 0) == 100);
    }
  }

  SECTION("copying values") {
    engine::ParameterManager manager_a(parameters);
    engine::ParameterManager manager_b(parameters);
    engine::ParameterManager manager_c(parameters);

    manager_a.set_capacity(5);
    manager_b.set_capacity(5);
    manager_c.set_capacity(5);

    auto resources = std::make_unique<hans_object_resource[]>(5);
    auto created = manager_a.make(resources.get(), 1, 0);

    manager_a.set(resources[0].parameter, 1, 10);
    manager_a.set(resources[1].parameter, 0, 11);

    manager_c.copy(manager_a);
    manager_a.copy(manager_b);
    manager_b.copy(manager_c);

    REQUIRE(manager_b.get(resources[0].parameter, 1) == 10);
    REQUIRE(manager_b.get(resources[1].parameter, 0) == 11);
  }
}
