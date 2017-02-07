#include <fstream>
#include <iostream>
#include "hans/engine/engine.hpp"
#include "hans/engine/primitives.hpp"
#include "hans/engine/serialize.hpp"
#include "hans/pipeline/input.hpp"
#include "hans/pipeline/output.hpp"

using namespace hans;

int main(int argc, char* argv[]) {
  pipeline::user_data input;
  EngineData data;

  if (!pipeline::input(argv[1], input)) {
    std::cerr << "[HANS] Unable to load config file" << std::endl;
    return 1;
  }
  if (!pipeline::output(input, data)) {
    std::cerr << "[HANS] Unable to process config file" << std::endl;
    return 1;
  }

  {
    std::ofstream fs(argv[2]);
    cereal::XMLOutputArchive ar(fs);
    ar(data);
  }

  engine::Engine engine(data);
  engine.setup();
  engine.run_forever();
  engine.destroy();
  return 0;
}
