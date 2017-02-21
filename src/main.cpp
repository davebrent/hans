#include <atomic>
#include <deque>
#include <efsw/efsw.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include <thread>
#include "hans/audio_backend.hpp"
#include "hans/engine.hpp"
#include "hans/image.hpp"
#include "hans/interpreter.hpp"
#include "hans/primitives.hpp"
#include "hans/sequencer.hpp"
#include "hans/serialize.hpp"
#include "hans/tasks.hpp"
#include "hans/user_config_compiler.hpp"
#include "hans/user_config_loader.hpp"
#include "hans/video.hpp"
#include "hans/window.hpp"

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
  DirectoryListener(user_reload& reload, std::deque<Command>& commands)
      : _reload(reload),
        _frames(_reload.delay),
        _last_seen(0),
        _buffer(commands) {
    _modified.store(0);
  }

  virtual void handleFileAction(efsw::WatchID watchid, const std::string& dir,
                                const std::string& filename,
                                efsw::Action action,
                                std::string oldfilename = "") override {
    if (action != efsw::Actions::Modified) {
      return;
    }

    for (const auto& exclude : _reload.exclude) {
      // FIXME: Does not exclude sub-directories
      if (dir.compare(exclude) == 0) {
        return;
      }
    }

    for (const auto& extension : _reload.extensions) {
      if (filename.length() < extension.length()) {
        return;
      }

      auto endswith = filename.compare(filename.length() - extension.length(),
                                       extension.length(), extension) == 0;
      if (endswith) {
        _modified++;
        return;
      }
    }
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
      _frames = _reload.delay;
      return;
    }

    _last_seen = 0;
    _modified = 0;
    _frames = _reload.delay;
    _buffer.push_back(Command(Command::RELOAD));
  }

 private:
  user_reload& _reload;
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
  std::mutex mutex;

  EngineReloader(const EngineReloader& other) = delete;

  EngineReloader(EngineData& data, AudioBuses& buses) {
    _bytes = std::calloc(2, sizeof(Engine));
    _place = 0;
    _instance = new (_bytes) Engine(data, buses);
  }

  ~EngineReloader() {
    std::free(_bytes);
  }

  Engine* get() {
    return _instance;
  }

  void close() {
    _instance->~Engine();
  }

  Engine* swap(EngineData& data, AudioBuses& buses) {
    _place = (_place + 1) % 2;
    auto place = static_cast<char*>(_bytes) + (_place * sizeof(Engine));
    _instance->~Engine();
    _instance = new (place) Engine(data, buses);
    return _instance;
  }

 private:
  void* _bytes;
  int _place;
  Engine* _instance;
};

//

static void command_dump(const EngineData& data) {
  std::ofstream fs("hans.xml");
  cereal::XMLOutputArchive ar(fs);
  ar(data);
}

static void command_reload(const char* filepath, EngineReloader& reloader,
                           AudioBuses& buses, Engine** ptr,
                           Sequencer& sequencer, TaskQueue& task_queue) {
  user_data input;
  EngineData output;

  if (!hans::load_config(filepath, input)) {
    error("Failed to reload: bad config file");
    return;
  }

  if (!hans::compile_config(input, output)) {
    error("Failed to reload: bad data");
    return;
  }

  {
    std::lock_guard<std::mutex> lck(reloader.mutex);
    task_queue.block_and_clear();
    sequencer.reload(output.sequences);
    *ptr = reloader.swap(output, buses);
    task_queue.unblock();
  }

  info("Successfully reloaded");
}

static void command_screenshot(Window& window, Frame& frame) {
  window.capture(frame);
  image::encode("hans.png", frame);
}

static void sequencer_handler(EngineReloader& reloader, const Track& track,
                              float val, bool state) {
  {
    std::lock_guard<std::mutex> lck(reloader.mutex);
    auto engine = reloader.get();
    engine->set_parameter(track.object, track.parameter, track.component, val);
  }
}

int realtime_mode(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "usage: " << argv[0] << " <config>" << std::endl;
    return 0;
  }

  auto config = argv[1];
  user_data input;
  EngineData output;
  if (!hans::load_config(config, input)) {
    error("Unable to load config file");
    return 1;
  }

  if (!hans::compile_config(input, output)) {
    error("Unable to process config file");
    return 1;
  }

  TaskQueue task_queue;
  std::deque<Command> commands;

  // Engine

  Frame frame(output.settings.width, output.settings.height);
  Engine* engine = nullptr;
  AudioBuses buses(output.settings, 1);
  Window window;
  if (!window.make("Hans", input.settings.width, input.settings.height)) {
    error("Unable to open window");
    return 1;
  }

  auto audio = make_audio_backend(output.settings, buses, [&]() {
    if (engine != nullptr) {
      engine->tick_audio();
    }
  });

  if (!audio) {
    error("Unknown audio backend");
    return 1;
  }

  // Watcher

  efsw::FileWatcher filewatcher;
  DirectoryListener listener(input.reload, commands);
  for (const auto& path : input.reload.paths) {
    filewatcher.addWatch(path, &listener, true);
  }
  filewatcher.watch();

  KeyControls keycontrols(commands);
  window.set_key_controls([&](int key) { keycontrols.process(key); });

  EngineReloader reloader(output, buses);
  engine = reloader.get();

  Sequencer sequencer(task_queue, output.sequences,
                      [&](const Track& track, float value, bool state) {
                        sequencer_handler(reloader, track, value, state);
                      });

  if (!audio->open()) {
    error("Unable to open audio backend");
    return 1;
  }

  std::thread sequencer_thread(&Sequencer::run_forever, &sequencer);
  std::thread background_thread(&TaskQueue::run_forever, &task_queue);

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
        if (command.value < engine->data().programs.names.size()) {
          engine->set_program(command.value);
          sequencer.set_program(command.value);
        } else {
          info("Program out of range");
        }
        break;
      case Command::DUMP:
        command_dump(engine->data());
        break;
      case Command::RELOAD:
        command_reload(config, reloader, buses, &engine, sequencer, task_queue);
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
        audio->start();
        break;
      case Command::AUDIO_OFF:
        audio->stop();
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

  task_queue.stop();
  background_thread.join();

  sequencer.stop();
  sequencer_thread.join();

  reloader.close();
  audio->close();
  return 0;
}

int render_mode(int argc, char* argv[]) {
  auto data_path = argv[1];
  auto output_path = argv[2];

  EngineData output;
  std::ifstream fs(data_path);
  cereal::PortableBinaryInputArchive ar(fs);
  ar(output);

  auto width = output.settings.width;
  auto height = output.settings.height;

  AudioBuses buses(output.settings, 1);
  Window window;
  if (!window.make("Hans - Render", width, height)) {
    error("Unable to open window");
    return 1;
  }

  // FIXME:
  auto num_frames = 60 * 10;
  std::ofstream os(output_path, std::ios::binary);
  VideoEncoder video(os, num_frames, width, height);
  Engine engine(output, buses);
  Frame frame(width, height);

  for (auto i = 0; i < num_frames; ++i) {
    engine.tick_graphics();
    window.capture(frame);
    video.encode(frame);
    window.update();
  }

  return 0;
}

int main(int argc, char* argv[]) {
  switch (argc) {
  case 2:
    return realtime_mode(argc, argv);
  case 3:
    return render_mode(argc, argv);
  default:
    std::cout << "usage: hans <config>" << std::endl;
    std::cout << "       hans <data> <output>" << std::endl;
    return 1;
  }
}
