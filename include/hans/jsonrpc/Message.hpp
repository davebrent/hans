#ifndef HANS_JSONRPC_MESSAGE_H_
#define HANS_JSONRPC_MESSAGE_H_

#include <json.hpp>
#include <vector>
#include <zmq.hpp>
#include "hans/common/types.hpp"

namespace hans {
namespace jsonrpc {

class Message {
 public:
  Message();

  /// Set the requests raw data
  void set(std::string raw);

  /// Update the message's document with new data
  bool parse();

  /// Returns true if the message is a valid jsonrpc message
  bool valid();

  /// Pack the data into the message
  void pack();

  /// Unpack the data in the message
  void unpack();

  /// Clears the messages data
  void clear();

  void set_parse_error();
  void set_invalid();
  void set_runtime_error(const std::string& message,
                         const nlohmann::json& data);
  void set_internal_error(const std::string& error);
  void set_method_not_found(const std::string& method);
  void set_invalid_params(const std::vector<std::string>& params);

  bool notification() const;
  std::string method() const;

  nlohmann::json m_data;
  zmq::message_t m_message;

 private:
  std::string m_method;
  std::string m_raw;
  bool m_notification;
  bool m_valid;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_MESSAGE_H_
