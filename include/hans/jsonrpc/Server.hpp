#ifndef HANS_JSONRPC_SERVER_H_
#define HANS_JSONRPC_SERVER_H_

#include "hans/common/config.hpp"
#include "hans/memory/StringManager.hpp"
#include <zmq.hpp>

#include "./Handler.hpp"
#include "./Message.hpp"

namespace hans {
namespace jsonrpc {

class Server {
 public:
  Server(hans::memory::StringManager& string_manager,
         hans_rpc_server_parameters& config);

  /// Returns true if a message has been received
  bool receive(hans::jsonrpc::Message& message);

  /// Send a message back to a client
  void send(hans::jsonrpc::Message& message);

  /// Start processing N messages
  void update(hans::jsonrpc::IHandler& handler);

  bool process(hans::jsonrpc::IHandler& handler,
               hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  hans::memory::StringManager& m_string_manager;
  uint16_t m_messages_per_frame;

  zmq::context_t m_context;
  zmq::socket_t m_socket;

  hans::jsonrpc::Message m_request;
  hans::jsonrpc::Message m_response;
  hans::jsonrpc::Message m_blank;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_SERVER_H_
