#include "./Midi.hpp"
#include <RtMidi.h>
#include <libguile.h>
#include <new>
#include "hans/common/procedure.hpp"

static scm_t_bits MidiOutTag;

static RtMidiOut* scm_to_midi_out_device(SCM device) {
  scm_assert_smob_type(MidiOutTag, device);
  return reinterpret_cast<RtMidiOut*>(SCM_SMOB_DATA(device));
}

static SCM make_midi_out() {
  auto place = scm_gc_malloc(sizeof(RtMidiOut), "midi-out-device");
  auto device = new (place) RtMidiOut();
  return scm_new_smob(MidiOutTag, (scm_t_bits)device);
}

static SCM midi_out_ports(SCM device) {
  auto midi_out = scm_to_midi_out_device(device);
  auto output = SCM_EOL;

  auto num_ports = midi_out->getPortCount();
  for (auto i = 0; i < num_ports; ++i) {
    auto name = midi_out->getPortName(i).c_str();
    auto value = scm_from_locale_string(name);
    output = scm_append(scm_list_2(output, scm_list_1(value)));
  }

  return output;
}

static SCM midi_out_open(SCM device, SCM index) {
  if (scm_is_false(scm_integer_p(index))) {
    return SCM_BOOL_F;
  }

  auto midi_out = scm_to_midi_out_device(device);
  midi_out->openPort(scm_to_int(index));
  return SCM_BOOL_T;
}

static SCM midi_out_send(SCM device, SCM byte1, SCM byte2, SCM byte3) {
  auto midi_out = scm_to_midi_out_device(device);
  auto message = std::vector<unsigned char>();
  message.reserve(3);
  message.push_back(scm_to_int(byte1));
  message.push_back(scm_to_int(byte2));
  message.push_back(scm_to_int(byte3));
  midi_out->sendMessage(&message);
  return SCM_BOOL_T;
}

static SCM midi_out_close(SCM device) {
  auto midi_out = scm_to_midi_out_device(device);
  midi_out->~RtMidiOut();
  return SCM_BOOL_T;
}

void hans::connect::init_midi_module() {
  MidiOutTag = scm_make_smob_type("midi-out-device", sizeof(RtMidiOut));
  scm::procedure<make_midi_out>("make-midi-out");
  scm::procedure<midi_out_ports>("midi-out-ports", 1, 0, 0);
  scm::procedure<midi_out_open>("midi-out-open", 2, 0, 0);
  scm::procedure<midi_out_send>("midi-out-send", 4, 0, 0);
  scm::procedure<midi_out_close>("midi-out-close", 1, 0, 0);
}
