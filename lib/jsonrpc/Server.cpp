#include "hans/jsonrpc/Server.hpp"
#include <sstream>
#include <stdexcept>
#include <cassert>
#include <string>

using namespace hans;

jsonrpc::Server::Server(memory::StringManager& string_manager,
                        hans_rpc_server_parameters& config)
    : m_string_manager(string_manager),
      m_messages_per_frame(config.requests_per_frame),
      m_context(config.num_threads),
      m_socket(m_context, ZMQ_REP) {
  m_socket.bind("tcp://*:" + std::to_string(config.port));
}

bool jsonrpc::Server::receive(jsonrpc::Message& request) {
  bool has = m_socket.recv(&request.m_message, ZMQ_NOBLOCK);

  if (has) {
    char* buff = static_cast<char*>(request.m_message.data());
    request.set(std::string(buff, request.m_message.size()));
  }

  return has;
}

void jsonrpc::Server::send(jsonrpc::Message& message) {
  m_socket.send(message.m_message);
}

bool jsonrpc::Server::process(jsonrpc::IHandler& handler,
                              jsonrpc::Message& request,
                              jsonrpc::Message& response) {
  std::string method;
  bool should_respond = true;

  if (!request.parse()) {
    response.set_parse_error();
    return true;
  }

  request.unpack();

  should_respond = !request.notification();

  if (should_respond) {
    response.clear();
    response.m_data["jsonrpc"] = "2.0";
    response.m_data["id"] = request.m_data["id"];
  }

  if (request.valid() == false) {
    if (should_respond) {
      response.set_invalid();
      return true;
    }
    return false;
  }

  method = request.method();
  hans_hash method_hash = m_string_manager.intern(method.c_str());

  if (!handler.has_method(method_hash)) {
    if (should_respond) {
      response.set_method_not_found(method);
      return true;
    }
    return false;
  }

  try {
    handler.call_method(method_hash, request, response);
  } catch (std::exception& e) {
    if (should_respond) {
      response.set_internal_error(e.what());
    }
  }

  return should_respond;
}

void jsonrpc::Server::update(jsonrpc::IHandler& handler) {
  for (int i = 0; i < m_messages_per_frame; ++i) {
    if (!receive(m_request)) {
      break;
    }

    if (process(handler, m_request, m_response)) {
      m_response.pack();
      send(m_response);
    } else {
      send(m_blank);
    }
  }
}
