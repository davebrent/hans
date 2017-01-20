#include "hans/engine/RingBufferManager.hpp"
#include <catch.hpp>
#include "hans/common/primitives.hpp"

using namespace hans;
using namespace hans::engine;
using namespace hans::common;

TEST_CASE("ring buffer manager", "[ringbuffer]") {
  SECTION("writing data to multiple ring buffers") {
    RingBuffer rb1;
    rb1.name = 0x10;
    rb1.producer = 1;
    rb1.index = 0;

    RingBuffer rb2;
    rb2.name = 0x20;
    rb2.producer = 2;
    rb2.index = 1;

    std::vector<RingBuffer> ringbuffers = {rb1, rb2};

    RingBufferManager manager(4, ringbuffers);

    audio::sample frame1[4] = {13, 14, 15, 16};
    audio::sample frame2[4] = {23, 24, 25, 26};

    manager.write(rb1, &frame1[0]);
    manager.write(rb2, &frame2[0]);

    auto f1 = manager.read(rb1.name, 0);
    REQUIRE(f1[0] == 13);
    REQUIRE(f1[1] == 14);
    REQUIRE(f1[2] == 15);
    REQUIRE(f1[3] == 16);

    auto f2 = manager.read(rb2.name, 0);
    REQUIRE(f2[0] == 23);
    REQUIRE(f2[1] == 24);
    REQUIRE(f2[2] == 25);
    REQUIRE(f2[3] == 26);
  }
}
