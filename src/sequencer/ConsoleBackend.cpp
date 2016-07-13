#include "./ConsoleBackend.hpp"
#include <iostream>

using namespace hans;

void sequencer::ConsoleBackend::send(const sequencer::clock_event& clock) {
  switch (clock.status) {
  case sequencer::START:
    std::cout << "CLOCK status=start" << std::endl;
    break;

  case sequencer::STOP:
    std::cout << "CLOCK status=stop" << std::endl;
    break;

  case sequencer::TICK:
    std::cout << "CLOCK status=tick" << std::endl;
    break;

  case sequencer::CONTINUE:
    std::cout << "CLOCK status=continue" << std::endl;
    break;
  }
}

void sequencer::ConsoleBackend::send(const sequencer::time_event& time) {
  std::cout << "TIME ppb=" << time.ppb << " pulse=" << time.pulse
            << " beat=" << time.beat << " bar=" << time.bar << std::endl;
}

void sequencer::ConsoleBackend::send(const sequencer::note_event& note) {
  std::cout << "NOTE pitch=" << note.pitch << " velocity=" << note.velocity
            << " channel=" << note.channel << " duration=" << note.duration
            << std::endl;
}

void sequencer::ConsoleBackend::send(const sequencer::ctrl_event& event) {
  std::cout << "CTRL value=" << event.value << " target=" << event.target
            << " duration=" << event.duration << std::endl;
}
