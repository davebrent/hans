#include "hans/engine/ParameterManager.hpp"
#include <catch.hpp>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

TEST_CASE("parameter manager", "[parameters]") {
  SECTION("making parameters for multiple object instances") {
    Parameter::Value data[2] = {10, 20};
    Parameter fixture[2];
    fixture[0].object = 1;
    fixture[0].name = 0x10;
    fixture[0].size = 1;
    fixture[0].offset = 0;

    fixture[1].object = 2;
    fixture[1].name = 0x10;
    fixture[1].size = 1;
    fixture[1].offset = 1;

    auto parameters = ListView<Parameter>(&fixture[0], 2);
    auto values = ListView<Parameter::Value>(&data[0], 2);
    auto manager = ParameterManager(parameters, values);

    auto handle1 = manager.make(1, 0x10);
    auto handle2 = manager.make(2, 0x10);

    REQUIRE(manager.get(handle1, 0) == 10);
    REQUIRE(manager.get(handle2, 0) == 20);
  }
}
