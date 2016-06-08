#ifndef HANS_COMMON_LOGGER_H_
#define HANS_COMMON_LOGGER_H_

namespace hans {
namespace common {

class Logger {
 public:
  enum level { DEBUG, INFO, ERROR };
  virtual void log(level l, const char* msg) {
  }
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_LOGGER_H_
