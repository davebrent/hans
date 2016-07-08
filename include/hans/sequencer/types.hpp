#ifndef HANS_SEQUENCER_TYPES_H_
#define HANS_SEQUENCER_TYPES_H_

#include <stdint.h>

namespace hans {
namespace sequencer {

enum clock_status { START, STOP, CONTINUE, TICK };

typedef struct {
  uint32_t ppb; // ticks per beat
  uint32_t pulse;
  uint32_t beat;
  uint32_t bar;
} time_event;

typedef struct { clock_status status; } clock_event;

typedef struct {
  float pitch;
  float velocity;
  float duration;
  uint16_t channel;
} note_event;

typedef struct {
  uint16_t controller;
  float value;
  float target;
  float duration;
  uint16_t channel;
  uint32_t revision;
} ctrl_event;

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_TYPES_H_
