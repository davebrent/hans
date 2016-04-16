#ifndef HANS_COMMON_LOGGING_H_
#define HANS_COMMON_LOGGING_H_

#include <iostream>
#include <vector>
#include "hans/common/Logger.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace common {

class StreamLogger : public virtual hans::common::Logger {
 public:
  StreamLogger(Logger::level level, std::ostream& stream);
  void log(Logger::level level, const char* msg);
  void log(Logger::level level, const std::vector<hans_library>& libraries);
  void log(Logger::level level, const std::vector<hans_object>& objects);
  void log(Logger::level level, const hans::common::ObjectGraph& graph);

 private:
  Logger::level m_level;
  std::ostream& m_stream;
};

class ConsoleLogger : public virtual hans::common::Logger {
 public:
  explicit ConsoleLogger(Logger::level level);
  void log(Logger::level level, const char* msg);
  void log(Logger::level level, const std::vector<hans_library>& libraries);
  void log(Logger::level level, const std::vector<hans_object>& objects);
  void log(Logger::level level, const hans::common::ObjectGraph& graph);

 private:
  StreamLogger m_logger;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_LOGGING_H_
