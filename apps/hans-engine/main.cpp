#include <libguile.h>
#include <cxxopts.hpp>
#include <iostream>
#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/AudioDevices.hpp"
#include "hans/audio/AudioStream.hpp"
#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/DataLoader.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/hasher.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/ProgramManager.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/graphics/Window.hpp"

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

static int run(const char* filepath, hans_hash program, hans_config& config) {
  DataReader reader(filepath);
  auto d = reader.data;

  hans_object_api object_api;

  auto strings = StringManager(d.string_hashes, d.string_offsets, d.strings);
  auto libraries = LibraryManager(strings, d.objects);
  auto programs = ProgramManager(object_api);
  auto registers = RegisterManager(config);
  auto parameters = ParameterManager();
  auto modulators = ModulationManager(parameters, d.modulators);
  auto window = Window("Hans", config.width, config.height);
  auto shaders = ShaderManager(strings, d.shaders);
  auto fbos = FrameBufferManager(d.fbos, d.fbo_attachments);
  auto audio_devices = AudioDevices();
  auto audio_buffers = AudioBufferManager(d.audio_buffers);
  auto audio_buses = AudioBusManager(config, 1);
  auto audio_stream = AudioStream(config, audio_devices, audio_buses, programs);
  auto ring_buffers = RingBufferManager(config, d.ring_buffers);

  object_api.config = &config;
  object_api.strings = &strings;
  object_api.parameters = &parameters;
  object_api.audio_buses = &audio_buses;
  object_api.audio_buffers = &audio_buffers;
  object_api.ring_buffers = &ring_buffers;
  object_api.shaders = &shaders;
  object_api.registers = &registers;
  object_api.fbos = &fbos;
  object_api.modulators = &modulators;

  libraries.load(d.libraries);
  parameters.use(d.parameters, d.parameter_values);
  modulators.setup();
  registers.use(d.registers);
  programs.use(d.objects, d.programs, d.chains, d.object_data);
  programs.setup_all();
  programs.switch_to(program);

  if (!audio_stream.open()) {
    std::cerr << "Unable to open audio stream" << std::endl;
  }

  audio_stream.start();

  while (!window.should_close()) {
    programs.tick_graphics(0);
    ring_buffers.advance_all();
    window.update();
  }

  programs.close_all();
  audio_stream.close();
  return 0;
}

void inner_main(void* closure, int argc, char* argv[]) {
  cxxopts::Options options(argv[0], " <file>");

  hans_config config;
  config.channels = 2;
  config.samplerate = 44100;
  config.blocksize = 64;
  config.width = 640;
  config.height = 360;

  // clang-format off
  options.add_options()
    ("p,program", "Initial program to run", cxxopts::value<std::string>())
    ("h,help", "Show this screen")
    ("width", "Screen width [default: 640]",
     cxxopts::value<uint16_t>(config.width), "PX")
    ("height", "Screen height [default: 360]",
     cxxopts::value<uint16_t>(config.height), "PX")
    ("channels", "Audio channels [default: 2]",
     cxxopts::value<uint8_t>(config.channels), "N")
    ("samplerate", "Target sample rate [default: 44100]",
     cxxopts::value<uint16_t>(config.samplerate), "HZ")
    ("buffersize", "Buffer size [default: 256]",
     cxxopts::value<uint16_t>(config.blocksize), "SAMPS");

  options.add_options("Hidden")
    ("positional", "Positional arguments (internal)",
     cxxopts::value<std::vector<std::string>>());
  // clang-format on

  options.parse_positional("positional");

  try {
    options.parse(argc, argv);
  } catch (const cxxopts::OptionException& e) {
    std::cout << "error parsing options: " << e.what() << std::endl;
    return;
  }

  auto args = options["positional"].as<std::vector<std::string>>();

  if (options.count("help") || args.size() == 0) {
    std::cout << options.help({""}) << std::endl;
    return;
  }

  hans_hash program = 0;
  if (options.count("program") != 0) {
    program = hasher(options["program"].as<std::string>().c_str());
  }

  run(args.at(0).c_str(), program, config);
};

int main(int argc, char** argv) {
  scm_boot_guile(argc, argv, inner_main, nullptr);
}
