#ifndef HANS_JSONRPC_HANDLER_H_
#define HANS_JSONRPC_HANDLER_H_

#include "hans/common/types.hpp"
#include "hans/common/Logging.hpp"
#include "hans/jsonrpc/Message.hpp"
#include "hans/jsonrpc/Method.hpp"
#include <vector>
#include <cstdint>

namespace hans {
namespace jsonrpc {

class IHandler {
 public:
  virtual void call_method(hans_hash name, const jsonrpc::Message& request,
                           jsonrpc::Message& response) {
  }
  virtual void add_method(hans_hash name, hans::jsonrpc::Method* method) {
  }
  virtual bool has_method(hans_hash name) {
    return false;
  }
};

class Handler : public virtual IHandler {
 public:
  void add_method(hans_hash name, hans::jsonrpc::Method* method);
  bool has_method(hans_hash name);
  void call_method(hans_hash name, const jsonrpc::Message& request,
                   jsonrpc::Message& response);

 private:
  std::vector<hans_hash> m_lookup;
  std::vector<hans::jsonrpc::Method*> m_method;
};

class LoggingHandler : public virtual IHandler {
 public:
  LoggingHandler(IHandler& next, hans::common::Logger& logger);
  void add_method(hans_hash name, hans::jsonrpc::Method* method);
  bool has_method(hans_hash name);
  void call_method(hans_hash name, const jsonrpc::Message& request,
                   jsonrpc::Message& response);

 private:
  IHandler& m_next;
  hans::common::Logger& m_logger;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_HANDLER_H_
