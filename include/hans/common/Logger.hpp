#ifndef HANS_COMMON_LOGGER_H_
#define HANS_COMMON_LOGGER_H_

#include "hans/common/types.hpp"
#include <vector>

namespace hans {
namespace common {

class ObjectGraph;

class Logger {
 public:
  enum level { DEBUG, INFO, ERROR };
  virtual void log(level l, const char* msg) {
  }
  virtual void log(level l, const std::vector<hans_library>& libraries) {
  }
  virtual void log(level l, const std::vector<hans_object>& objects) {
  }
  virtual void log(level l, const hans::common::ObjectGraph& graph) {
  }
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_LOGGER_H_
