#include "hans/engine/ModulationManager.hpp"
#include <catch.hpp>
#include "hans/common/primitives.hpp"

using namespace hans;
using namespace hans::engine;

TEST_CASE("modulation manager", "[modulators]") {
  SECTION("modulating parameters") {
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

    Modulator modulator;
    modulator.source.object = 1;
    modulator.source.parameter = 0x10;
    modulator.source.component = 0;

    modulator.dest.object = 2;
    modulator.dest.parameter = 0x10;
    modulator.dest.component = 0;

    modulator.offset = 3;
    modulator.scale = 1.5;

    std::vector<Parameter> parameters = {p1, p2};
    std::vector<Parameter::Value> values = {10, 20};
    std::vector<Modulator> modulators = {modulator};

    engine::ParameterManager param_manager(parameters, values);
    engine::ModulationManager mod_manager(param_manager, modulators);

    auto handle2 = param_manager.make(2, 0x10);

    mod_manager.begin();
    REQUIRE(param_manager.get(handle2, 0) == 39.5); // (10 + 3) * 1.5
    mod_manager.end();

    REQUIRE(param_manager.get(handle2, 0) == 20);
  }
}
