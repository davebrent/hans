#include "./Sequencer.hpp"

extern "C" {
void scm_init_sequencer_module() {
  hans::sequencer::init_sequencer_module();
}
}
