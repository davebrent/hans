#include "hans/memory/StringManager.hpp"
#include <iostream>
#include <cxxopts.hpp>

using namespace hans;

int main(int argc, char* argv[]) {
  cxxopts::Options options(argv[0], " <args>...");

  // clang-format off
  options.add_options()
    ("h,help", "Show this screen");

  options.add_options("Hidden")
    ("positional", "Positional arguments (internal)",
     cxxopts::value<std::vector<std::string>>());
  // clang-format on

  options.parse_positional("positional");

  try {
    options.parse(argc, argv);
  } catch (const cxxopts::OptionException& e) {
    std::cout << "error parsing options: " << e.what() << std::endl;
    return 1;
  }

  auto args = options["positional"].as<std::vector<std::string>>();

  if (options.count("help") || args.size() == 0) {
    std::cout << options.help({""}) << std::endl;
    return 0;
  }

  memory::StringManager s(16384 /* 16kb */);
  for (auto& arg : args) {
    std::cout << "0x" << std::hex << s.intern(arg) << std::endl;
  }

  return 0;
};
