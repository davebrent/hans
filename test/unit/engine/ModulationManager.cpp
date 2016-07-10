#include "hans/engine/ModulationManager.hpp"
#include <catch.hpp>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

using namespace hans;

TEST_CASE("modulation manager", "[modulators]") {
  SECTION("modulating parameters") {
    hans_parameter_value data[2] = {10, 20};
    hans_parameter fixture[2];

    fixture[0].object = 1;
    fixture[0].name = 0x10;
    fixture[0].size = 1;
    fixture[0].offset = 0;

    fixture[1].object = 2;
    fixture[1].name = 0x10;
    fixture[1].size = 1;
    fixture[1].offset = 1;

    hans_modulator modulator;
    modulator.source.object = 1;
    modulator.source.parameter = 0x10;
    modulator.source.component = 0;

    modulator.dest.object = 2;
    modulator.dest.parameter = 0x10;
    modulator.dest.component = 0;

    modulator.offset = 3;
    modulator.scale = 1.5;

    auto parameters = common::ListView<hans_parameter>(&fixture[0], 2);
    auto values = common::ListView<hans_parameter_value>(&data[0], 2);
    auto modulators = common::ListView<hans_modulator>(&modulator, 1);

    auto param_manager = engine::ParameterManager();
    auto mod_manager = engine::ModulationManager(param_manager, modulators);

    param_manager.use(parameters, values);
    mod_manager.setup();

    auto handle1 = param_manager.make(1, 0x10);
    auto handle2 = param_manager.make(2, 0x10);

    mod_manager.begin();
    REQUIRE(param_manager.get(handle2, 0) == 39.5); // (10 + 3) * 1.5
    mod_manager.end();

    REQUIRE(param_manager.get(handle2, 0) == 20);
  }
}
