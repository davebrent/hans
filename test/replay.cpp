#include "hans/replay.hpp"
#include <catch.hpp>
#include "hans/primitives.hpp"

using namespace hans;

TEST_CASE("replay recorder", "[replay]") {
  SECTION("recording and replaying parameters") {
    std::vector<Parameter::Value> values = {10, 20};
    Recordings recordings;

    ReplayRecorder recorder(values, recordings);
    ReplayPlayer player(values, recordings);

    recorder.start();
    recorder.tick();
    values[0] = 11.f;
    values[1] = 21.f;
    recorder.tick();
    recorder.stop();

    values[0] = 0;
    values[1] = 0;

    player.start();
    player.tick();
    REQUIRE(values[0] == 10.0f);
    REQUIRE(values[1] == 20.0f);
    player.tick();
    REQUIRE(values[0] == 11);
    REQUIRE(values[1] == 21);

    // Ensure an error is not thrown
    player.tick();
    REQUIRE(values[0] == 11);
    REQUIRE(values[1] == 21);

    player.set(1, 12);
    player.tick();
    REQUIRE(values[0] == 11);
    REQUIRE(values[1] == 21);

    player.set(0, 0);
    player.tick();
    REQUIRE(values[0] == 10);
    REQUIRE(values[1] == 20);
  }
}
