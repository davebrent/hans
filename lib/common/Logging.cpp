#include "hans/common/Logging.hpp"
#include <chrono>
#include <iomanip>
#include <sstream>

using namespace hans;

static std::string format_level(common::Logger::level level) {
  switch (level) {
  case common::Logger::DEBUG:
    return "[DEBUG]";
  case common::Logger::INFO:
    return "[INFO]";
  case common::Logger::ERROR:
    return "[ERROR]";
  }
}

static std::string format_time() {
  auto now = std::chrono::system_clock::now();
  auto in_time_t = std::chrono::system_clock::to_time_t(now);

  std::stringstream ss;
  ss << std::put_time(std::localtime(&in_time_t), "%H:%M:%S");
  return ss.str();
}

static void write_msg(std::ostream& stream, common::Logger::level level,
                      const char* msg) {
  stream << format_time() << " " << format_level(level) << " " << msg
         << std::endl;
}

common::StreamLogger::StreamLogger(Logger::level level, std::ostream& stream)
    : m_stream(stream) {
  m_level = level;
}

void common::StreamLogger::log(Logger::level level, const char* msg) {
  if (level >= m_level) {
    write_msg(m_stream, level, msg);
  }
}

common::ConsoleLogger::ConsoleLogger(Logger::level level)
    : m_logger(level, std::cout) {
}

void common::ConsoleLogger::log(Logger::level level, const char* msg) {
  m_logger.log(level, msg);
}
