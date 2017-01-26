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
    modulator.source = 0;
    modulator.destination = 1;
    modulator.offset = 3;
    modulator.scale = 1.5;

    Parameters parameters;
    parameters.split = 0;
    parameters.handles = {p1, p2};
    parameters.buffer = {10, 20};

    Modulation modulation;
    modulation.graphics.local = {modulator};
    engine::ModulationManager mod_manager(modulation, parameters);

    ParameterManager param_manager(parameters.handles, parameters.buffer);
    auto handle2 = param_manager.make(2, 0x10);

    mod_manager.gfx_modulate();
    REQUIRE(param_manager.get(handle2, 0) == 39.5); // (10 + 3) * 1.5
    mod_manager.gfx_restore();

    REQUIRE(param_manager.get(handle2, 0) == 20);
  }

  SECTION("double buffer") {
    modulation::DoubleBuffer buffer(2);

    buffer.write({10, 20});
    buffer.swap();
    auto data = buffer.load();
    REQUIRE(data[0] == 10);
    REQUIRE(data[1] == 20);

    buffer.write({30, 40});
    buffer.swap();
    data = buffer.load();
    REQUIRE(data[0] == 30);
    REQUIRE(data[1] == 40);

    buffer.write({50, 60});
    buffer.swap();
    data = buffer.load();
    REQUIRE(data[0] == 50);
    REQUIRE(data[1] == 60);
  }

  SECTION("ring buffer") {
    modulation::RingBuffer buffer(2, 3); // [[0 0] [0 0] [0 0]]

    buffer.write({10, 11});
    buffer.write({20, 21});
    auto available = buffer.available();
    REQUIRE(available == 2);

    auto data = buffer.load();
    REQUIRE(data[0] == 10);
    REQUIRE(data[1] == 11);

    data = buffer.load();
    REQUIRE(data[0] == 20);
    REQUIRE(data[1] == 21);
  }

  SECTION("cross thread modulation") {
    Parameter p1 = {/*object*/ 1, /*name*/ 0x20, /*size*/ 2, /*offset*/ 0};
    Parameter p2 = {/*object*/ 2, /*name*/ 0x10, /*size*/ 2, /*offset*/ 2};

    Parameters parameters;
    parameters.split = 2;
    parameters.handles = {p1, p2};
    parameters.buffer = {11, 12, 13, 14};

    Modulator m1 = {/*source*/ 0, /*destination*/ 2, /*offset*/ 0,
                    /*scale*/ 3.5};
    Modulator m2 = {/*source*/ 3, /*destination*/ 1, /*offset*/ 1,
                    /*scale*/ 1.5};

    Modulation modulation;
    modulation.audio.cross = {m1};
    modulation.graphics.cross = {m2};

    engine::ModulationManager mmanager(modulation, parameters);
    ParameterManager pmanager(parameters.handles, parameters.buffer);

    auto snd = pmanager.make(1, 0x20);
    auto gfx = pmanager.make(2, 0x10);

    mmanager.snd_modulate();
    mmanager.gfx_modulate();
    mmanager.snd_restore();
    REQUIRE(pmanager.get(gfx, 0) == 51.5); // 13 + ((11 + 0) * 3.5)
    mmanager.gfx_restore();

    REQUIRE(pmanager.get(snd, 0) == 11);
    REQUIRE(pmanager.get(snd, 1) == 12);
    REQUIRE(pmanager.get(gfx, 0) == 13);
    REQUIRE(pmanager.get(gfx, 1) == 14);
  }
}
