#include "hans/common/Logging.hpp"
#include "hans/common/ObjectGraph.hpp"
#include <sstream>
#include <iomanip>
#include <chrono>

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

void common::StreamLogger::log(Logger::level level,
                               const std::vector<hans_library>& libraries) {
  if (level >= m_level) {
    for (auto& library : libraries) {
      std::ostringstream ss;

      ss << "LIBRARY name=" << library.name
         << " handle=" << static_cast<void*>(library.handle)
         << " filepath=" << library.filepath;

      write_msg(m_stream, level, ss.str().c_str());
    }
  }
}

void common::StreamLogger::log(Logger::level level,
                               const std::vector<hans_object>& objects) {
  if (level < m_level) {
    return;
  }

  for (auto& object : objects) {
    std::string type;

    switch (object.type) {
    case HANS_AUDIO:
      type = "audio";
      break;
    case HANS_GRAPHICS:
      type = "graphics";
      break;
    case HANS_UNKOWN:
      type = "unknown";
      break;
    default:
      type = "error";
      break;
    }

    std::ostringstream ss;

    ss << "OBJECT id=" << object.id << " name=" << object.name
       << " size=" << object.size << " type=" << type
       << " make=" << reinterpret_cast<void*>(object.make)
       << " destroy=" << reinterpret_cast<void*>(object.destroy);

    write_msg(m_stream, level, ss.str().c_str());
  }
}

void common::StreamLogger::log(Logger::level level,
                               const common::ObjectGraph& graph) {
  if (level < m_level) {
    return;
  }

  auto connections = graph.get_connections();
  auto arguments = graph.get_arguments();

  std::ostringstream header;

  header << "GRAPH objects=" << graph.get_objects().size()
         << " connections=" << connections.size()
         << " arguments=" << arguments.size();
  write_msg(m_stream, level, header.str().c_str());
  m_stream << std::endl;

  std::ostringstream ss;

  for (const auto& object : graph.get_objects()) {
    ss << "\tOBJECT "
       << "object_id=" << object.object_id
       << " instance_id=" << object.instance_id
       << " arguments_index=" << object.arguments_index
       << " arguments_len=" << object.arguments_len
       << " arg_ptr=" << object.arguments << " inlets=" << object.inlets
       << " outlets=" << object.outlets << std::endl;
  }

  for (const auto& connection : connections) {
    ss << "\tCONNECTION "
       << "source=" << connection.source << " sink=" << connection.sink
       << std::endl;
  }

  for (const auto& argument : arguments) {
    ss << "\tARG "
       << " name=" << argument.name;
    switch (argument.type) {
    case HANS_BOOL:
      ss << " value=" << argument.boolean << std::endl;
      break;
    case HANS_NUMBER:
      ss << " value=" << argument.number << std::endl;
      break;
    case HANS_STRING:
      ss << " value=" << argument.string << std::endl;
      break;
    }
  }

  m_stream << ss.str() << std::endl;
}

common::ConsoleLogger::ConsoleLogger(Logger::level level)
    : m_logger(level, std::cout) {
}

void common::ConsoleLogger::log(Logger::level level, const char* msg) {
  m_logger.log(level, msg);
}

void common::ConsoleLogger::log(Logger::level level,
                                const std::vector<hans_library>& libraries) {
  m_logger.log(level, libraries);
}

void common::ConsoleLogger::log(Logger::level level,
                                const std::vector<hans_object>& objects) {
  m_logger.log(level, objects);
}

void common::ConsoleLogger::log(Logger::level level,
                                const hans::common::ObjectGraph& graph) {
  m_logger.log(level, graph);
}
