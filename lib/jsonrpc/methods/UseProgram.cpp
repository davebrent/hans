#include "hans/jsonrpc/methods/UseProgram.hpp"

using namespace hans;

jsonrpc::UseProgram::UseProgram(memory::StringManager& string_manager,
                                engine::ProgramManager& program_manager)
    : m_string_manager(string_manager), m_program_manager(program_manager) {
}

void jsonrpc::UseProgram::execute(const jsonrpc::Message& request,
                                  jsonrpc::Message& response) {
  nlohmann::json params = request.m_data["params"];
  nlohmann::json name_json = params["name"];

  if (!name_json.is_string()) {
    response.set_invalid_params({"name"});
    return;
  }

  auto name = name_json.get<std::string>();
  auto result = m_program_manager.use(m_string_manager.intern(name.c_str()));
  response.m_data["result"] = result;
}
