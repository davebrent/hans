#include <libguile.h>
#include <chrono>
#include <iostream>
#include <thread>
#include "./ConsoleBackend.hpp"
#include "./MidiOutBackend.hpp"
#include "hans/sequencer/Device.hpp"
#include "hans/sequencer/types.hpp"

using namespace hans;

typedef struct {
  SCM handler;    // Procedure responsible for invoking generators
  SCM interval;   // Time between each handler call
  SCM generators; // List of generator procedures
} scheduler_data;

static scm_t_bits console_device_tag;
static scm_t_bits midi_out_device_tag;
static scm_t_bits scheduler_tag;

static SCM EVENT_NOTE_SYMBOL;
static SCM EVENT_CTRL_SYMBOL;
static SCM EVENT_TIME_SYMBOL;
static SCM EVENT_START_SYMBOL;
static SCM EVENT_STOP_SYMBOL;
static SCM EVENT_CONTINUE_SYMBOL;
static SCM EVENT_CLOCK_SYMBOL;
static SCM EVENT_TICK_SYMBOL;

static SCM make_midi_out_device(SCM name) {
  void* place = scm_gc_malloc(sizeof(sequencer::Device), "midi-out-device");
  const char* midi_device_name = scm_to_locale_string(name);
  auto backend = new sequencer::MidiOutBackend(midi_device_name);
  auto device = new (place) sequencer::Device(backend);
  SCM smob = scm_new_smob(midi_out_device_tag, (scm_t_bits)device);
  return smob;
}

static size_t free_midi_out_device(SCM device) {
  scm_assert_smob_type(midi_out_device_tag, device);
  auto instance = (sequencer::Device*)SCM_SMOB_DATA(device);
  instance->~Device();
  return 0;
}

static int print_midi_out_device(SCM smob, SCM port, scm_print_state* pstate) {
  scm_assert_smob_type(midi_out_device_tag, smob);
  scm_puts("#<midi-out-device>", port);
  return 1;
}

static SCM make_console_device() {
  void* place = scm_gc_malloc(sizeof(sequencer::Device), "console-device");
  auto device = new (place) sequencer::Device(new sequencer::ConsoleBackend());
  SCM smob = scm_new_smob(console_device_tag, (scm_t_bits)device);
  return smob;
}

static size_t free_console_device(SCM device) {
  scm_assert_smob_type(console_device_tag, device);
  auto instance = (sequencer::Device*)SCM_SMOB_DATA(device);
  instance->~Device();
  return 0;
}

static int print_console(SCM smob, SCM port, scm_print_state* pstate) {
  scm_assert_smob_type(console_device_tag, smob);
  scm_puts("#<console-device>", port);
  return 1;
}

static SCM make_scheduler(SCM interval, SCM handler) {
  auto size = sizeof(scheduler_data);
  auto scheduler = (scheduler_data*)scm_gc_malloc(size, "scheduler");
  scheduler->interval = SCM_BOOL_F;
  scheduler->handler = SCM_BOOL_F;
  SCM smob = scm_new_smob(scheduler_tag, (scm_t_bits)scheduler);
  scheduler->interval = interval;
  scheduler->handler = handler;
  scheduler->generators = scm_list_n(SCM_UNDEFINED);
  return smob;
}

static int print_scheduler(SCM smob, SCM port, scm_print_state* pstate) {
  scm_assert_smob_type(scheduler_tag, smob);
  auto scheduler = (scheduler_data*)SCM_SMOB_DATA(smob);
  scm_puts("#<scheduler ", port);
  scm_display(scm_length(scheduler->generators), port);
  scm_puts(" ", port);
  scm_display(scheduler->interval, port);
  scm_puts(">", port);
  return 1;
}

static SCM schedule(SCM scheduler, SCM generators) {
  scm_assert_smob_type(scheduler_tag, scheduler);
  auto instance = (scheduler_data*)SCM_SMOB_DATA(scheduler);
  instance->generators =
      scm_append(scm_list_2(instance->generators, generators));
  return scheduler;
}

static SCM flush_device(SCM device) {
  auto instance = (sequencer::Device*)SCM_SMOB_DATA(device);
  instance->flush();
  return device;
}

static SCM send_event(SCM device, SCM data) {
  // Handle data not being a list
  if (scm_is_true(scm_list_p(data)) == 0) {
    return device;
  }

  // Handle device not being a device instance
  // scm_assert_smob_type(midi_out_device_tag, device);
  auto device_instance = (sequencer::Device*)SCM_SMOB_DATA(device);

  // Handle data not being the correct length
  int len = scm_to_int(scm_length(data));
  if (len <= 0) {
    return device;
  }

  // Type of data
  auto type = scm_list_ref(data, scm_from_int(0));

  if (scm_is_true(scm_eq_p(type, EVENT_NOTE_SYMBOL)) == 1) {
    if (len != 5) {
      return device;
    }

    sequencer::note_event note;
    note.pitch = scm_to_double(scm_list_ref(data, scm_from_int(1)));
    note.velocity = scm_to_double(scm_list_ref(data, scm_from_int(2)));
    note.duration = scm_to_double(scm_list_ref(data, scm_from_int(3)));
    note.channel = scm_to_int(scm_list_ref(data, scm_from_int(4)));
    device_instance->send(note);
  } else if (scm_is_true(scm_eq_p(type, EVENT_CTRL_SYMBOL)) == 1) {
    if (len != 6) {
      return device;
    }

    sequencer::ctrl_event event;
    event.controller = scm_to_int(scm_list_ref(data, scm_from_int(1)));
    event.value = scm_to_double(scm_list_ref(data, scm_from_int(2)));
    event.target = scm_to_double(scm_list_ref(data, scm_from_int(3)));
    event.duration = scm_to_double(scm_list_ref(data, scm_from_int(4)));
    event.channel = scm_to_int(scm_list_ref(data, scm_from_int(5)));
    device_instance->send(event);
  } else if (scm_is_true(scm_eq_p(type, EVENT_CLOCK_SYMBOL)) == 1) {
    auto msg = scm_list_ref(data, scm_from_int(1));

    sequencer::clock_event event;
    if (scm_is_true(scm_eq_p(msg, EVENT_START_SYMBOL)) == 1) {
      event.status = sequencer::START;
    } else if (scm_is_true(scm_eq_p(msg, EVENT_STOP_SYMBOL)) == 1) {
      event.status = sequencer::STOP;
    } else if (scm_is_true(scm_eq_p(msg, EVENT_CONTINUE_SYMBOL)) == 1) {
      event.status = sequencer::CONTINUE;
    } else if (scm_is_true(scm_eq_p(msg, EVENT_TICK_SYMBOL)) == 1) {
      event.status = sequencer::TICK;
    } else {
      return device;
    }

    device_instance->send(event);
  } else if (scm_is_true(scm_eq_p(type, EVENT_TIME_SYMBOL)) == 1) {
    sequencer::time_event event;
    event.ppb = scm_to_int(scm_list_ref(data, scm_from_int(1)));
    event.pulse = scm_to_int(scm_list_ref(data, scm_from_int(2)));
    event.beat = scm_to_int(scm_list_ref(data, scm_from_int(3)));
    event.bar = scm_to_int(scm_list_ref(data, scm_from_int(4)));
    device_instance->send(event);
  }

  return device;
}

static SCM run_scheduler(SCM device, SCM scheduler) {
  using namespace std::chrono;

  scm_assert_smob_type(scheduler_tag, scheduler);

  auto device_instance = (sequencer::Device*)SCM_SMOB_DATA(device);
  auto scheduler_instance = (scheduler_data*)SCM_SMOB_DATA(scheduler);

  auto interval = scm_to_double(scheduler_instance->interval);
  auto resolution = microseconds(320 * 2);
  auto start = high_resolution_clock::now();
  auto dev_start = high_resolution_clock::now();

  while (true) {
    auto end = high_resolution_clock::now();
    auto delta = duration_cast<microseconds>(end - start).count() / 1000.f;
    auto dev_delta =
        duration_cast<microseconds>(end - dev_start).count() / 1000.f;
    dev_start = high_resolution_clock::now();

    if (delta < interval) {
      device_instance->tick(dev_delta);
      std::this_thread::sleep_for(resolution);
      continue;
    }

    start = high_resolution_clock::now();

    auto scm_delta = scm_from_double(delta);
    auto generators = scm_call_3(scheduler_instance->handler, scm_delta,
                                 scheduler_instance->generators, device);

    if (scm_is_true(scm_list_p(generators)) == 0 ||
        scm_to_int(scm_length(generators)) == 0) {
      return scheduler;
    }

    scheduler_instance->generators = generators;
    device_instance->tick(dev_delta);
    std::this_thread::sleep_for(resolution);
  }

  return scheduler;
}

extern "C" {
void scm_init_sequencer_module() {
  scm_c_define_gsubr("make-console-device", 0, 0, 0,
                     (scm_t_subr)make_console_device);
  scm_c_define_gsubr("make-midi-out-device", 1, 0, 0,
                     (scm_t_subr)make_midi_out_device);
  scm_c_define_gsubr("make-scheduler", 2, 0, 0, (scm_t_subr)make_scheduler);
  scm_c_define_gsubr("send-event", 2, 0, 0, (scm_t_subr)send_event);
  scm_c_define_gsubr("schedule", 2, 0, 0, (scm_t_subr)schedule);
  scm_c_define_gsubr("run", 2, 0, 0, (scm_t_subr)run_scheduler);
  scm_c_define_gsubr("flush", 1, 0, 0, (scm_t_subr)flush_device);

  auto sizeof_console = sizeof(sequencer::ConsoleBackend);
  console_device_tag = scm_make_smob_type("console-device", sizeof_console);
  scm_set_smob_print(console_device_tag, print_console);
  scm_set_smob_free(console_device_tag, free_console_device);

  scheduler_tag = scm_make_smob_type("scheduler", sizeof(scheduler_data));
  scm_set_smob_print(scheduler_tag, print_scheduler);

  auto sizeof_midi = sizeof(sequencer::MidiOutBackend);
  midi_out_device_tag = scm_make_smob_type("midi-out-device", sizeof_midi);
  scm_set_smob_print(midi_out_device_tag, print_midi_out_device);
  scm_set_smob_free(midi_out_device_tag, free_midi_out_device);

  EVENT_START_SYMBOL = scm_from_locale_symbol("start");
  EVENT_STOP_SYMBOL = scm_from_locale_symbol("stop");
  EVENT_CONTINUE_SYMBOL = scm_from_locale_symbol("continue");
  EVENT_TICK_SYMBOL = scm_from_locale_symbol("tick");
  EVENT_CLOCK_SYMBOL = scm_from_locale_symbol("clock");
  EVENT_TIME_SYMBOL = scm_from_locale_symbol("time");
  EVENT_NOTE_SYMBOL = scm_from_locale_symbol("note");
  EVENT_CTRL_SYMBOL = scm_from_locale_symbol("ctrl");
}
}
