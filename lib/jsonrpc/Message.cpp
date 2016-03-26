#include "hans/jsonrpc/Message.hpp"
#include <exception>

using json = nlohmann::json;
using namespace hans;

jsonrpc::Message::Message() {
  m_notification = true;
  m_valid = false;
}

bool jsonrpc::Message::parse() {
  try {
    m_data = json::parse(m_raw);
    return true;
  } catch (std::exception& e) {
    return false;
  }
}

void jsonrpc::Message::pack() {
  std::string raw = m_data.dump();
  m_message.rebuild(raw.size());
  std::memcpy(m_message.data(), raw.c_str(), raw.size());
}

void jsonrpc::Message::unpack() {
  try {
    m_data["id"].get<int>();
    m_notification = false;
  } catch (std::exception& e) {
    m_notification = true;
  }

  m_method = "";

  if (!m_data.is_object() || m_data.find("method") == m_data.end()) {
    m_valid = false;
    return;
  }

  try {
    m_method = m_data["method"].get<std::string>();
  } catch (std::exception& e) {
    m_valid = false;
  }

  m_valid = true;
}

void jsonrpc::Message::set(std::string raw) {
  m_raw = raw;
}

bool jsonrpc::Message::valid() {
  return m_valid;
}

std::string jsonrpc::Message::method() const {
  return m_method;
}

bool jsonrpc::Message::notification() const {
  return m_notification;
}

void jsonrpc::Message::clear() {
  m_data.clear();
}

void jsonrpc::Message::set_parse_error() {
  clear();
  m_data["jsonrpc"] = "2.0";
  m_data["id"] = nullptr;
  m_data["error"]["code"] = "-32700";
  m_data["error"]["message"] = "Parse error (-32700)";
}

void jsonrpc::Message::set_runtime_error(const std::string& message,
                                         const json& error) {
  m_data["error"]["code"] = "-32000";
  m_data["error"]["message"] = message;
  m_data["error"]["data"] = error;
}

void jsonrpc::Message::set_invalid() {
  m_data["error"]["code"] = "-32600";
  m_data["error"]["message"] = "Invalid request (-32600)";
}

void jsonrpc::Message::set_method_not_found(const std::string& method) {
  m_data["error"]["code"] = "-32601";
  m_data["error"]["message"] = "Method not found (-32601)";
  m_data["error"]["data"]["method"] = method;
}

void jsonrpc::Message::set_invalid_params(
    const std::vector<std::string>& params) {
  m_data["error"]["code"] = "-32602";
  m_data["error"]["message"] = "Invalid parameters (-32602)";
  m_data["error"]["data"] = params;
}

void jsonrpc::Message::set_internal_error(const std::string& err) {
  m_data["error"]["code"] = "-32603";
  m_data["error"]["message"] = "Internal error (-32603)";
  m_data["error"]["data"]["message"] = err;
}
