#include "hans/sequencer/Device.hpp"
#include <catch.hpp>
#include <iostream>
#include <vector>
#include "hans/sequencer/Backend.hpp"
#include "hans/sequencer/types.hpp"

using namespace hans;

class TestBackend : public virtual sequencer::Backend {
 public:
  std::vector<sequencer::note_event> note_ons;
  std::vector<sequencer::note_event> note_offs;
  std::vector<sequencer::ctrl_event> controls;

  void send(const sequencer::note_event& note) {
    if (note.velocity > 0) {
      note_ons.push_back(note);
    } else {
      note_offs.push_back(note);
    }
  }

  void send(const sequencer::ctrl_event& event) {
    controls.push_back(event);
  }
};

TEST_CASE("sequencer device", "[sequencer]") {
  SECTION("sending note on and note off events") {
    TestBackend* backend = new TestBackend();
    sequencer::Device device(backend);
    sequencer::note_event n = {64, 127, 1000, 1};

    device.send(n);
    REQUIRE(backend->note_ons.size() == 1);
    device.tick(500);
    REQUIRE(backend->note_offs.size() == 0);

    device.tick(500);
    REQUIRE(backend->note_ons.size() == 1);
    REQUIRE(backend->note_offs.size() == 1);
  }

  SECTION("removing events") {
    TestBackend* backend = new TestBackend();
    sequencer::Device device(backend);
    sequencer::note_event note = {64, 127, 1000, 1};

    device.send(note);
    REQUIRE(backend->note_ons.size() == 1);
    REQUIRE(device.pending() == true);
    device.tick(500);
    REQUIRE(device.pending() == true);
    device.tick(500);
    REQUIRE(device.pending() == false);
  }

  SECTION("sending control events") {
    TestBackend* backend = new TestBackend();
    sequencer::Device device(backend);
    // Go from 0 - 60 in 1 second
    sequencer::ctrl_event e = {1, 0, 60, 1000, 1};

    device.send(e);
    device.tick(100); // <- This tick should not effect the value

    REQUIRE(backend->controls.at(0).value == 0);
    device.tick(250);
    REQUIRE(backend->controls.at(1).value == 15.0);
    device.tick(250);
    REQUIRE(backend->controls.at(2).value == 30.0);
    device.tick(250);
    REQUIRE(backend->controls.at(3).value == 45.0);
    device.tick(250);
    REQUIRE(backend->controls.at(4).value == 60.0);
    REQUIRE(device.pending() == false);
  }

  SECTION("sending control events in a negative direction") {
    TestBackend* backend = new TestBackend();
    sequencer::Device device(backend);
    // Go from 0 - 60 in 1 second
    sequencer::ctrl_event e = {1, 60, 0, 1000, 1};

    device.send(e);
    device.tick(100); // <- This tick should not effect the value

    REQUIRE(backend->controls.at(0).value == 60);
    device.tick(250);
    REQUIRE(backend->controls.at(1).value == 45.0);
    device.tick(250);
    REQUIRE(backend->controls.at(2).value == 30.0);
    device.tick(250);
    REQUIRE(backend->controls.at(3).value == 15.0);
    device.tick(250);
    REQUIRE(backend->controls.at(4).value == 0.0);
    REQUIRE(device.pending() == false);
  }
}
