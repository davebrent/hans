#include "./Midi.hpp"

extern "C" {
void scm_init_connect_module() {
  hans::connect::init_midi_module();
}
}
