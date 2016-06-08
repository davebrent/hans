#ifndef HANS_COMMON_LOGGING_H_
#define HANS_COMMON_LOGGING_H_

#include <iostream>
#include "hans/common/Logger.hpp"

namespace hans {
namespace common {

class StreamLogger : public virtual hans::common::Logger {
 public:
  StreamLogger(Logger::level level, std::ostream& stream);
  void log(Logger::level level, const char* msg);

 private:
  Logger::level m_level;
  std::ostream& m_stream;
};

class ConsoleLogger : public virtual hans::common::Logger {
 public:
  explicit ConsoleLogger(Logger::level level);
  void log(Logger::level level, const char* msg);

 private:
  StreamLogger m_logger;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_LOGGING_H_
