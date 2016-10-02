#include "hans/engine/RingBufferManager.hpp"
#include <catch.hpp>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

using namespace hans;
using namespace hans::engine;
using namespace hans::common;

TEST_CASE("ring buffer manager", "[ringbuffer]") {
  SECTION("writing data to multiple ring buffers") {
    Config config;
    config.blocksize = 4;

    RingBuffer fixture[2];
    fixture[0].name = 0x10;
    fixture[0].producer = 1;
    fixture[1].name = 0x20;
    fixture[1].producer = 2;

    auto list = ListView<RingBuffer>(&fixture[0], 2);
    auto manager = RingBufferManager(config, list);

    audio::sample frame1[4] = {13, 14, 15, 16};
    audio::sample frame2[4] = {23, 24, 25, 26};

    manager.write(fixture[0], &frame1[0]);
    manager.write(fixture[1], &frame2[0]);

    auto f1 = manager.read(fixture[0].name, 0);
    REQUIRE(f1[0] == 13);
    REQUIRE(f1[1] == 14);
    REQUIRE(f1[2] == 15);
    REQUIRE(f1[3] == 16);

    auto f2 = manager.read(fixture[1].name, 0);
    REQUIRE(f2[0] == 23);
    REQUIRE(f2[1] == 24);
    REQUIRE(f2[2] == 25);
    REQUIRE(f2[3] == 26);
  }
}
