#ifndef HANS_JSONRPC_METHODS_USEPROGRAM_H_
#define HANS_JSONRPC_METHODS_USEPROGRAM_H_

#include "hans/common/types.hpp"
#include "hans/engine/ProgramManager.hpp"
#include "hans/jsonrpc/Method.hpp"

namespace hans {
namespace jsonrpc {

class UseProgram : public virtual hans::jsonrpc::Method {
 public:
  explicit UseProgram(hans::memory::StringManager& string_manager,
                      hans::engine::ProgramManager& program_manager);
  void execute(const hans::jsonrpc::Message& request,
               hans::jsonrpc::Message& response);

 private:
  hans::memory::StringManager& m_string_manager;
  hans::engine::ProgramManager& m_program_manager;
};

} // namespace jsonrpc
} // namespace hans

#endif // HANS_JSONRPC_METHODS_USEPROGRAM_H_
