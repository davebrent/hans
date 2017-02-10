#include <libguile.h>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>

#include "hans/scm/procedure.hpp"
#include "hans/scm/smobs.hpp"
#include "hans/sequencer/midi.hpp"
#include "hans/sequencer/sequencer.hpp"

using namespace hans;
using namespace hans::sequencer;

static SCM midi_out_open(SCM device, SCM index) {
  if (scm_is_false(scm_integer_p(index))) {
    return SCM_BOOL_F;
  }

  scm::to_cpp<MidiOut>(device).open(scm_to_int(index));
  return SCM_BOOL_T;
}

static SCM midi_out_send(SCM device, SCM byte1, SCM byte2, SCM byte3) {
  scm::to_cpp<MidiOut>(device).send(scm_to_int(byte1), scm_to_int(byte2),
                                    scm_to_int(byte3));
  return SCM_BOOL_T;
}

static SCM sequencer_track(SCM seq, SCM proc) {
  // FIXME: This value never gets freed
  proc = scm_gc_protect_object(proc);
  scm::to_cpp<Sequencer>(seq).add_track([proc](Cycle& cycle) -> EventList {
    auto duration = scm_from_double(cycle.duration.load());
    auto number = scm_from_size_t(cycle.number.load());
    return scm::to_cpp<EventList>(scm_call_2(proc, duration, number));
  });
  return SCM_BOOL_T;
}

extern "C" {
void init_scm_hans_seq() {
  scm::smob<MidiOut>("midi-out");
  scm::procedure<midi_out_open>("midi-out-open", 2, 0, 0);
  scm::procedure<midi_out_send>("midi-out-send", 4, 0, 0);

  scm::smob<Sequencer>("sequencer-realtime", [](void* bytes, SCM args) {
    auto procedure = scm_list_ref(args, scm_from_int(0));
    // FIXME: This value never gets freed
    scm_gc_protect_object(procedure);
    return new (bytes)
        Sequencer([procedure](size_t track, size_t value, bool state) {
          scm_call_3(procedure, scm_from_size_t(track), scm_from_size_t(value),
                     scm_from_bool(state));
        });
  });
}
}
