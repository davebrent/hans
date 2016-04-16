#ifndef HANS_JSONRPC_METHODS_GETSHADERS_H_
#define HANS_JSONRPC_METHODS_GETSHADERS_H_

#include "hans/common/types.hpp"
#include "hans/jsonrpc/Method.hpp"
#include "hans/memory/StringManager.hpp"

namespace hans {
namespace jsonrpc {

/// A command for listing available objects
class GetShaders : public virtual hans::jsonrpc::Method {
 public:
  explicit GetShaders(const hans::memory::StringManager& string_manager,
                      const std::vector<hans_shader>& shaders);
  void execute(const hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  const hans::memory::StringManager& m_string_manager;
  const std::vector<hans_shader>& m_shaders;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_GETSHADERS_H_
