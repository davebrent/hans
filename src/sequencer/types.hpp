#ifndef HANS_SEQUENCER_TYPES_H_
#define HANS_SEQUENCER_TYPES_H_

#include <libguile.h>
#include <vector>

namespace hans {
namespace sequencer {

struct Event {
  // The cycle the event should be dispatched in
  uint64_t cycle;
  // Start time in milliseconds relative to start of the cycle
  float start;
  // Duration of the event in milliseconds
  float duration;
  // Value of the event, to be passed back to scheme
  SCM value;
};

using EventList = std::vector<Event>;

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_TYPES_H_
