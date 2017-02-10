#ifndef HANS_SEQUENCER_PRIMITIVES_H_
#define HANS_SEQUENCER_PRIMITIVES_H_

#include <stddef.h>
#include <atomic>
#include <functional>
#include <vector>

namespace hans {
namespace sequencer {

struct Cycle {
  std::atomic<float> duration;
  std::atomic<size_t> number;

  Cycle() {
    duration.store(0);
    number.store(0);
  }

  Cycle(size_t number_) {
    duration.store(2000);
    number.store(number_);
  }

  Cycle(const Cycle& other) {
    duration.store(other.duration.load());
    number.store(other.number.load());
  }
};

struct Event {
  // The cycle the event should be dispatched in
  uint64_t cycle;
  // Start time in milliseconds relative to start of the cycle
  float start;
  // Duration of the event in milliseconds
  float duration;
  // Value of the event, to be passed back to scheme
  size_t value;
};

using EventList = std::vector<Event>;

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_PRIMITIVES_H_
