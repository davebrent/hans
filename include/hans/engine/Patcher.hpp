#ifndef HANS_ENGINE_PATCHER_H_
#define HANS_ENGINE_PATCHER_H_

#include <vector>
#include "hans/common/primitives.hpp"

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

  virtual std::vector<Argument> arguments() = 0;
  virtual void missing(const char* name) = 0;
  virtual void invalid(const char* name) = 0;
  virtual void request(Resources type, size_t value) = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PATCHER_H_
