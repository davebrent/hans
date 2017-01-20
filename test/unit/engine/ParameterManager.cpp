#include "hans/engine/ParameterManager.hpp"
#include <catch.hpp>
#include "hans/common/types.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

TEST_CASE("parameter manager", "[parameters]") {
  SECTION("making parameters for multiple object instances") {
    Parameter p1;
    p1.object = 1;
    p1.name = 0x10;
    p1.size = 1;
    p1.offset = 0;

    Parameter p2;
    p2.object = 2;
    p2.name = 0x10;
    p2.size = 1;
    p2.offset = 1;

    std::vector<Parameter> parameters;
    parameters.push_back(p1);
    parameters.push_back(p2);

    std::vector<Parameter::Value> values;
    values.push_back(10);
    values.push_back(20);

    ParameterManager manager(parameters, values);

    auto handle1 = manager.make(1, 0x10);
    auto handle2 = manager.make(2, 0x10);

    REQUIRE(manager.get(handle1, 0) == 10);
    REQUIRE(manager.get(handle2, 0) == 20);
  }

  SECTION("setting parameters from object id and parameter name") {
    Parameter p1;
    p1.object = 11;
    p1.name = 0x10;
    p1.size = 1;
    p1.offset = 0;

    Parameter p2;
    p2.object = 1;
    p2.name = 0x20;
    p2.size = 1;
    p2.offset = 1;

    std::vector<Parameter> parameters;
    parameters.push_back(p1);
    parameters.push_back(p2);

    std::vector<Parameter::Value> values;
    values.push_back(10);
    values.push_back(20);

    ParameterManager manager(parameters, values);

    manager.set(1, 0x20, 0, 21);
    auto handle = manager.make(1, 0x20);
    REQUIRE(manager.get(handle, 0) == 21);
  }
}
