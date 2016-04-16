#include "hans/jsonrpc/Handler.hpp"
#include <catch.hpp>
#include <json.hpp>
#include "hans/jsonrpc/Method.hpp"
#include "hans/memory/StringManager.hpp"

using namespace hans;

class TestCommand : public virtual jsonrpc::Method {
 public:
  int id;
  explicit TestCommand(int id_) : id(id_) {
  }
  void execute(const jsonrpc::Message& request, jsonrpc::Message& response) {
    response.m_data["id"] = id;
  }
};

SCENARIO("handling rpc methods", "[rpc]") {
  GIVEN("requests that should result in an error") {
    memory::StringManager s(256);
    jsonrpc::Handler handler;
    TestCommand cmd(1);
    handler.add_method(s.intern("test_method"), &cmd);

    WHEN("checking if the handler has an existing method") {
      THEN("it should return true") {
        REQUIRE(handler.has_method(s.intern("test_method")) == true);
      }
    }

    WHEN("executing an existing method") {
      jsonrpc::Message request;
      jsonrpc::Message response;

      request.set("{\"method\":\"test_method\"}");
      request.parse();
      request.unpack();

      handler.call_method(s.intern("test_method"), request, response);

      THEN("an invalid response should be returned") {
        REQUIRE(response.m_data["id"].get<int>() == 1);
      }
    }
  }
}
