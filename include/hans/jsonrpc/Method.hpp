#ifndef HANS_JSONRPC_METHOD_H_
#define HANS_JSONRPC_METHOD_H_

#include "hans/common/types.hpp"
#include "hans/jsonrpc/Message.hpp"
#include <json.hpp>

namespace hans {
namespace jsonrpc {

class Method {
 public:
  virtual void execute(const hans::jsonrpc::Message& request,
                       hans::jsonrpc::Message& response) {
  }
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHOD_H_
