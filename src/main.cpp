#include <atomic>
#include <deque>
#include <efsw/efsw.hpp>
#include <fstream>
#include <iostream>
#include "./engine/audio_backend_portaudio.hpp"
#include "hans/engine/engine.hpp"
#include "hans/engine/image.hpp"
#include "hans/engine/primitives.hpp"
#include "hans/engine/serialize.hpp"
#include "hans/engine/window.hpp"
#include "hans/pipeline/input.hpp"
#include "hans/pipeline/output.hpp"

using namespace hans;

static bool error(const char* msg) {
  std::cerr << "[HANS] " << msg << std::endl;
  return false;
}

static bool info(const char* msg) {
  std::cout << "[HANS] " << msg << std::endl;
  return true;
}

struct Command {
  enum Type {
    PROGRAM,
    DUMP,
    SCREENSHOT,
    RELOAD,
    START,
    STOP,
    RECORD_START,
    RECORD_STOP,
    AUDIO_ON,
    AUDIO_OFF,
  };

  Type type;
  size_t value;

  Command(Type _type) : type(_type), value(0) {
  }

  Command(Type _type, size_t _value) : type(_type), value(_value) {
  }
};

class DirectoryListener : public efsw::FileWatchListener {
 public:
  DirectoryListener(size_t delay, std::deque<Command>& commands)
      : _delay(delay), _frames(_delay), _last_seen(0), _buffer(commands) {
    _modified.store(0);
  }

  virtual void handleFileAction(efsw::WatchID watchid, const std::string& dir,
                                const std::string& filename,
                                efsw::Action action,
                                std::string oldfilename = "") override {
    if (dir.find(".git") != std::string::npos) {
      return;
    }
    _modified++;
  }

  void update() {
    auto current = _modified.load();
    if (current == 0) {
      return;
    }

    if (_last_seen != current) {
      _last_seen = current;
      return;
    }

    if (_frames != 0) {
      _frames--;
      return;
    }

    auto success = _modified.compare_exchange_weak(current, 0);
    if (!success) {
      _frames = _delay;
      return;
    }

    _last_seen = 0;
    _modified = 0;
    _frames = _delay;
    _buffer.push_back(Command(Command::RELOAD));
  }

 private:
  size_t _delay;
  size_t _frames;
  size_t _last_seen;
  std::atomic<int> _modified;
  std::deque<Command>& _buffer;
};

class KeyControls {
 public:
  enum Key {
    SPACE = 32, // pause/start
    R = 82,     // toggle record
    S = 83,     // screenshot
    D = 68,     // dump
    ZERO = 48,  // switch/program
    NINE = 57,  // switch/program
  };

  KeyControls(std::deque<Command>& buffer)
      : _buffer(buffer), _recording(false), _paused(false) {
  }

  void process(int key) {
    if (key >= ZERO && key <= NINE) {
      auto value = (key == ZERO) ? 9 : key - ZERO - 1;
      _buffer.push_back(Command(Command::PROGRAM, value));
      return;
    }

    switch (key) {
    case SPACE:
      if (_paused) {
        _paused = false;
        _buffer.push_back(Command(Command::START));
      } else {
        _paused = true;
        _buffer.push_back(Command(Command::STOP));
      }
      break;
    case R:
      if (_recording) {
        _recording = false;
        _buffer.push_back(Command(Command::RECORD_STOP));
      } else {
        _recording = true;
        _buffer.push_back(Command(Command::RECORD_START));
      }
      break;
    case S:
      _buffer.push_back(Command(Command::SCREENSHOT));
      break;
    case D:
      _buffer.push_back(Command(Command::DUMP));
      break;
    }
  }

 private:
  std::deque<Command>& _buffer;
  bool _recording;
  bool _paused;
};

class EngineReloader {
 public:
  EngineReloader(EngineData& data, engine::AudioBuses& buses) {
    _bytes = std::calloc(2, sizeof(engine::Engine));
    _place = 0;
    _instance = new (_bytes) engine::Engine(data, buses);
  }

  ~EngineReloader() {
    std::free(_bytes);
  }

  engine::Engine* get() {
    return _instance;
  }

  void close() {
    _instance->~Engine();
  }

  engine::Engine* swap(EngineData& data, engine::AudioBuses& buses) {
    _place = (_place + 1) % 2;
    auto place = static_cast<char*>(_bytes) + (_place * sizeof(engine::Engine));
    _instance->~Engine();
    _instance = new (place) engine::Engine(data, buses);
    return _instance;
  }

 private:
  void* _bytes;
  int _place;
  engine::Engine* _instance;
};

static void command_dump(const EngineData& data) {
  std::ofstream fs("hans.xml");
  cereal::XMLOutputArchive ar(fs);
  ar(data);
}

static void command_reload(const char* filepath, EngineReloader& reloader,
                           engine::AudioBuses& buses, engine::Engine** ptr) {
  pipeline::user_data input;
  EngineData output;

  if (!pipeline::input(filepath, input)) {
    error("Failed to reload: bad config file");
    return;
  }

  if (!pipeline::output(input, output)) {
    error("Failed to reload: bad data");
    return;
  }

  *ptr = reloader.swap(output, buses);
  info("Successfully reloaded");
}

static void command_screenshot(engine::Window& window, Frame& frame) {
  window.capture(frame);
  engine::image::encode("hans.png", frame);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "usage: " << argv[0] << " <config>" << std::endl;
    return 0;
  }

  auto config = argv[1];
  pipeline::user_data input;
  EngineData output;
  if (!pipeline::input(config, input)) {
    error("Unable to load config file");
    return 1;
  }

  if (!pipeline::output(input, output)) {
    error("Unable to process config file");
    return 1;
  }

  Frame frame(output.settings.width, output.settings.height);
  engine::Engine* engine = nullptr;
  engine::AudioBuses buses(output.settings, 1);
  engine::Window window;
  if (!window.make("Hans", input.settings.width, input.settings.height)) {
    error("Unable to open window");
    return 1;
  }

  engine::AudioBackendPortAudio audio(output.settings, buses, [&]() {
    if (engine != nullptr) {
      engine->tick_audio();
    }
  });

  std::deque<Command> commands;

  auto frame_reload_delay = 2;
  efsw::FileWatcher filewatcher;
  DirectoryListener listener(frame_reload_delay, commands);
  for (const auto& path : input.watchers) {
    filewatcher.addWatch(path, &listener, true);
  }
  filewatcher.watch();

  KeyControls keycontrols(commands);
  window.set_key_controls([&](int key) { keycontrols.process(key); });

  EngineReloader reloader(output, buses);
  engine = reloader.get();

  if (!audio.open()) {
    std::cerr << "[HANS] Unable to open audio stream" << std::endl;
    return 1;
  }

  bool should_wait = false;

  while (!window.should_close()) {
    listener.update();

    while (!commands.empty()) {
      auto command = commands.front();
      commands.pop_front();

      switch (command.type) {
      case Command::START:
        should_wait = false;
        break;
      case Command::STOP:
        should_wait = true;
        break;
      case Command::PROGRAM:
        engine->set_program(command.value);
        break;
      case Command::DUMP:
        command_dump(engine->data());
        break;
      case Command::RELOAD:
        command_reload(config, reloader, buses, &engine);
        break;
      case Command::SCREENSHOT:
        command_screenshot(window, frame);
        break;
      case Command::RECORD_START:
        engine->record_start();
        break;
      case Command::RECORD_STOP:
        engine->record_stop();
        break;
      case Command::AUDIO_ON:
        audio.start();
        break;
      case Command::AUDIO_OFF:
        audio.stop();
        break;
      }
    }

    engine->tick_graphics();

    if (!should_wait) {
      window.update();
    } else {
      window.update_wait();
    }
  }

  reloader.close();
  audio.close();
  return 0;
}
