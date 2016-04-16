#include <cxxopts.hpp>
#include <iostream>
#include "hans/audio/AudioDevices.hpp"
#include "hans/audio/AudioStream.hpp"
#include "hans/common/Logging.hpp"
#include "hans/engine/DataLoader.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/Program.hpp"
#include "hans/engine/ProgramManager.hpp"
#include "hans/engine/ProgramResources.hpp"
#include "hans/graphics/Window.hpp"
#include "hans/jsonrpc/Handler.hpp"
#include "hans/jsonrpc/Message.hpp"
#include "hans/jsonrpc/Server.hpp"
#include "hans/jsonrpc/methods.hpp"

using namespace hans;

static int run(const std::string& resources, hans_config& config) {
  memory::StringManager string_manager(16384 /* 16kb */);
  common::ConsoleLogger logger(common::Logger::DEBUG);
  engine::DataLoader data_loader(resources.c_str(), string_manager);
  audio::AudioBufferManager audio_buffer_manager(config.audio.block_size);
  audio::AudioBusManager audio_bus_manager(audio_buffer_manager);

  auto libraries = data_loader.get_libraries();
  auto objects = data_loader.get_objects();
  auto parameters = data_loader.get_parameters();
  auto shaders = data_loader.get_shaders();
  auto frame_buffers = data_loader.get_frame_buffers();

  engine::LibraryManager library_manager(string_manager, objects);
  library_manager.load_libraries(libraries);

  logger.log(common::Logger::DEBUG, libraries);
  logger.log(common::Logger::DEBUG, objects);

  auto graphics_objects = library_manager.filter_objects(HANS_GRAPHICS);
  auto audio_objects = library_manager.filter_objects(HANS_AUDIO);

  engine::ProgramResources program_resources;
  program_resources.config = &config;
  program_resources.strings = &string_manager;
  program_resources.logger = &logger;
  program_resources.graphics_objects = &graphics_objects;
  program_resources.audio_objects = &audio_objects;
  program_resources.parameters = &parameters;
  program_resources.shaders = &shaders;
  program_resources.frame_buffers = &frame_buffers;
  program_resources.audio_buses = &audio_bus_manager;
  program_resources.audio_buffers = &audio_buffer_manager;

  engine::ProgramManager program_manager(program_resources);

  audio::AudioDevices audio_devices;
  audio::AudioStream audio_stream(config.audio, audio_devices,
                                  audio_bus_manager, program_manager, logger);
  if (!audio_stream.open()) {
    logger.log(common::Logger::ERROR, "Unable to open audio stream");
  }

  jsonrpc::Server server(string_manager, config.rpc);
  jsonrpc::Handler handler;

  jsonrpc::AudioStart start_audio(audio_stream);
  jsonrpc::AudioStop stop_audio(audio_stream);
  jsonrpc::GetAudioDevices get_audio_devices(audio_devices);
  jsonrpc::GetObjects get_objects(string_manager, objects);
  jsonrpc::SetProgram set_program(string_manager, program_manager);
  jsonrpc::UseProgram use_program(string_manager, program_manager);

  handler.add_method(string_manager.intern("start_audio"), &start_audio);
  handler.add_method(string_manager.intern("stop_audio"), &stop_audio);
  handler.add_method(string_manager.intern("get_audio_devices"),
                     &get_audio_devices);
  handler.add_method(string_manager.intern("set_program"), &set_program);
  handler.add_method(string_manager.intern("use_program"), &use_program);
  jsonrpc::LoggingHandler logging_handler(handler, logger);

  graphics::Window window("hans", config.window.width, config.window.height);

  while (!window.should_close()) {
    server.update(logging_handler);
    program_manager.process_graphics();
    window.update();
  }

  // Calls object destructors explicitly before the libraries get released
  program_manager.destroy();
  audio_stream.close();
  data_loader.del_parameters(parameters);
  data_loader.del_frame_buffers(frame_buffers);
  return 0;
}

int main(int argc, char* argv[]) {
  cxxopts::Options options(argv[0], " <resources>");

  hans_audio_device_parameters audio_config;
  audio_config.num_channels = 2;
  audio_config.sample_rate = 44100;
  audio_config.block_size = 256;

  hans_graphics_window_parameters graphics_config;
  graphics_config.width = 1184;
  graphics_config.height = 640;

  hans_rpc_server_parameters rpc_config;
  rpc_config.port = 5555;
  rpc_config.num_threads = 1;
  rpc_config.requests_per_frame = 10;

  hans_config config;
  config.audio = audio_config;
  config.window = graphics_config;
  config.rpc = rpc_config;

  // clang-format off
  options.add_options()
    ("h,help", "Show this screen");

  options.add_options("RPC")
    ("rpc-port", "RPC port [default: 5555]",
     cxxopts::value<uint16_t>(config.rpc.port), "PORT")
    ("rpc-threads", "Number of threads [default: 1]",
     cxxopts::value<uint16_t>(config.rpc.num_threads), "THREADS")
    ("rpc-rpf", "Requests handled per frame [default: 10]",
     cxxopts::value<uint16_t>(config.rpc.requests_per_frame), "MESSAGES");

  options.add_options("Window")
    ("window-width", "Screen width [default: 1184]",
     cxxopts::value<uint16_t>(config.window.width), "PX")
    ("window-height", "Screen height [default: 640]",
     cxxopts::value<uint16_t>(config.window.height), "PX");

  options.add_options("Audio")
    ("audio-channels", "Audio channels [default: 2]",
     cxxopts::value<uint8_t>(config.audio.num_channels), "N")
    ("audio-samplerate", "Target sample rate [default: 44100]",
     cxxopts::value<uint16_t>(config.audio.sample_rate), "HZ")
    ("audio-buffersize", "Buffer size [default: 256]",
     cxxopts::value<uint16_t>(config.audio.block_size), "SAMPS");

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
    std::cout << options.help({"", "RPC", "Window", "Audio"}) << std::endl;
    return 0;
  }

  return run(args.at(0), config);
};
