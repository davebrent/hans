#include "hans/engine/replay.hpp"
#include <catch.hpp>
#include "hans/common/primitives.hpp"

using namespace hans;
using namespace hans::engine;

TEST_CASE("replay recorder", "[replay]") {
  SECTION("recording and replaying parameters") {
    /*
    Parameter::Value data[2] = {10, 20};
    auto values = std::vector<Parameter::Value>(&data[0], &data[1]);

    ReplayRecorder recorder(values);

    recorder.start();
    recorder.update();
    data[0] = 11.0f;
    data[1] = 21.0f;
    recorder.update();
    recorder.stop();

    data[0] = 0;
    data[1] = 0;

    ReplayPlayer player(values);
    player.reset_with_blob(recorder.to_blob());

    player.tick();
    REQUIRE(data[0] == 10.0f);
    REQUIRE(data[1] == 20.0f);
    player.tick();
    REQUIRE(data[0] == 11);
    REQUIRE(data[1] == 21);

    // Ensure an error is not thrown
    player.tick();
    REQUIRE(data[0] == 11);
    REQUIRE(data[1] == 21);
    */
  }
}
