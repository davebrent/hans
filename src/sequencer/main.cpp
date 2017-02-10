#include <iostream>
#include <sstream>
#include "hans/sequencer/interpreter.hpp"
#include "hans/sequencer/midi.hpp"
#include "hans/sequencer/sequencer.hpp"

using namespace hans::sequencer;

static void print(const Tree& tree) {
  if (tree.children.size() == 0) {
    if (tree.value == 0xffff) {
      std::cout << " . ";
    } else {
      std::cout << " " << tree.value << " ";
    }
    return;
  }
  std::cout << " [";
  for (const auto& child : tree.children) {
    print(child);
  }
  std::cout << "] ";
}

int main(int argc, char* argv[]) {
  std::stringstream ss(argv[1]);
  auto istack = hans::seq::compile(ss);

  /*
  MidiOut midi;
  midi.open(0);
  for (const auto& device : midi.devices) {
    std::cout << device.index << ": " << device.name << std::endl;
  }

  Sequencer seq([&](size_t track, size_t value, bool state) {
    std::cout << "track=" << track << " value=" << value << " state=" << state
              << std::endl;
  });

  seq.add_track([&](Cycle& cycle) {
    Interpreter itp(cycle, istack);
    hans::seq::interpret(itp);

    auto value = itp.dstack.pop();
    return hans::seq::to_events(itp.cycle, value.tree);
  });

  seq.run_forever();
  seq.join();
  */

  std::cout << "cycle,track,start,duration,value" << std::endl;
  for (auto i = 0; i < 4; ++i) {
    Cycle cycle(i);
    Interpreter itp(cycle, istack);
    hans::seq::interpret(itp);

    auto value = itp.dstack.pop();
    auto events = hans::seq::to_events(itp.cycle, value.tree);

    for (auto& e : events) {
      std::cout << cycle.number << "," << 0 << "," << e.start << ","
                << e.duration << "," << e.value << std::endl;
    }
  }

  return 0;
}
