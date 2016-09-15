#include "./Midi.hpp"

extern "C" {
void scm_init_control_module() {
  hans::control::init_midi_module();
}
}
