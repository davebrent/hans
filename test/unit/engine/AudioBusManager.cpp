#include "hans/engine/AudioBusManager.hpp"
#include <catch.hpp>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

TEST_CASE("audio bus manager", "[audiobus]") {
  SECTION("writing data to single bus multiple channels") {
    Config config;
    config.blocksize = 4;
    config.channels = 2;

    AudioBusManager manager(config, 1);
    auto bus = manager.make();

    audio::sample frame1[4] = {13, 14, 15, 16};
    audio::sample frame2[4] = {23, 24, 25, 26};

    manager.write(bus, 0, &frame1[0]);
    manager.write(bus, 1, &frame2[0]);

    auto f1 = manager.read(bus, 0);
    auto f2 = manager.read(bus, 1);

    REQUIRE(f1[0] == 13);
    REQUIRE(f1[1] == 14);
    REQUIRE(f1[2] == 15);
    REQUIRE(f1[3] == 16);

    REQUIRE(f2[0] == 23);
    REQUIRE(f2[1] == 24);
    REQUIRE(f2[2] == 25);
    REQUIRE(f2[3] == 26);
  }

  SECTION("writing data to multipe buses & multiple channels") {
    Config config;
    config.blocksize = 4;
    config.channels = 2;

    AudioBusManager manager(config, 2);
    auto bus1 = manager.make();
    auto bus2 = manager.make();

    audio::sample frame1[4] = {13, 14, 15, 16};
    audio::sample frame2[4] = {23, 24, 25, 26};
    audio::sample frame3[4] = {33, 34, 35, 36};
    audio::sample frame4[4] = {43, 44, 45, 46};

    manager.write(bus1, 0, &frame1[0]);
    manager.write(bus1, 1, &frame2[0]);
    manager.write(bus2, 0, &frame3[0]);
    manager.write(bus2, 1, &frame4[0]);

    auto f1 = manager.read(bus1, 0);
    auto f2 = manager.read(bus1, 1);
    auto f3 = manager.read(bus2, 0);
    auto f4 = manager.read(bus2, 1);

    REQUIRE(f1[3] == 16);
    REQUIRE(f2[3] == 26);
    REQUIRE(f3[3] == 36);
    REQUIRE(f4[3] == 46);
  }
}
