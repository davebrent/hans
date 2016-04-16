#ifndef HANS_JSONRPC_METHODS_GETPARAMETERS_H_
#define HANS_JSONRPC_METHODS_GETPARAMETERS_H_

#include <vector>
#include "hans/common/types.hpp"
#include "hans/jsonrpc/Method.hpp"
#include "hans/memory/StringManager.hpp"

namespace hans {
namespace jsonrpc {

/// Returns all parameters for an object definition
class GetParameters : public virtual hans::jsonrpc::Method {
 public:
  GetParameters(const std::vector<hans_parameter>& parameters,
                const hans::memory::StringManager& string_manager);
  void execute(const hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  const std::vector<hans_parameter>& m_parameters;
  const hans::memory::StringManager& m_string_manager;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_GETPARAMETERS_H_
