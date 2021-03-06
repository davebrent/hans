#ifndef HANS_CONFIGURATOR_H_
#define HANS_CONFIGURATOR_H_

#include <vector>
#include "hans/primitives.hpp"

namespace hans {

class IConfigurator {
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

} // namespace hans

#endif // HANS_CONFIGURATOR_H_
