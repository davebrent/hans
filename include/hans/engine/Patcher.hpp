#ifndef HANS_ENGINE_PATCHER_H_
#define HANS_ENGINE_PATCHER_H_

#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class IPatcher {
 public:
  enum Resources {
    PARAMETER,
    SHADER,
    AUDIO_BUFFER,
    FRAME_BUFFER,
    INLET,
    OUTLET,
    RING_BUFFER
  };

  virtual common::ListView<Argument> arguments() = 0;
  virtual void missing(Argument::Types type, hash name) = 0;
  virtual void request(Resources type, size_t value) = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PATCHER_H_
