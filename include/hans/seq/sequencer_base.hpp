#ifndef HANS_SEQ_SEQUENCERBASE_H_
#define HANS_SEQ_SEQUENCERBASE_H_

#include <functional>
#include "hans/seq/primitives.hpp"

namespace hans {
namespace seq {

class SequencerBase {
 public:
  using Processor = std::function<void(void*)>;
  using Callback = std::function<EventList(Cycle&)>;
  using Handler = std::function<void(size_t, size_t, bool)>;

  virtual size_t add_track(float duration, Callback track) = 0;
  virtual bool start(Processor producer, Processor consumer) = 0;
  virtual bool stop() = 0;
};

} // namespace seq
} // namespace hans

#endif // HANS_SEQ_SEQUENCERBASE_H_
