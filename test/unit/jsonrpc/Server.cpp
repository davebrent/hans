#include "hans/jsonrpc/Server.hpp"
#include <catch.hpp>
#include <json.hpp>

using namespace hans;

SCENARIO("remote procedural calls", "[rpc]") {
  GIVEN("requests that should result in an error") {
    memory::StringManager s(128);

    // port, requests_per_frame, num_threads
    hans_rpc_server_parameters c = {5555, 1, 1};
    jsonrpc::Server server(s, c);
    jsonrpc::Handler handler;
    jsonrpc::Message request;
    jsonrpc::Message response;

    WHEN("an invalid request is received") {
      request.set("{\"jsonrpc\":\"2.0\",\"id\":1,\"params\":{\"a\":\"b\"}}");
      server.process(handler, request, response);
      THEN("an invalid response should be returned") {
        REQUIRE(response.m_data["error"]["code"] == "-32600");
      }
    }

    WHEN("malformed json is received") {
      request.set("*");
      server.process(handler, request, response);
      THEN("a parse error response should be returned") {
        REQUIRE(response.m_data["error"]["code"] == "-32700");
      }
    }

    WHEN("an unknown method is requested") {
      request.set("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"foo\"}");
      server.process(handler, request, response);
      THEN("a method not found error should be returned") {
        REQUIRE(response.m_data["error"]["code"] == "-32601");
      }
    }

    WHEN("an invalid notification is received") {
      request.set("{\"jsonrpc\":\"2.0\"}");
      server.process(handler, request, response);
      THEN("a method not found error should be returned") {
        REQUIRE(response.m_data.empty() == true);
      }
    }

    WHEN("an unknown method notification is received") {
      request.set("{\"jsonrpc\":\"2.0\", \"method\": \"foo\"}");
      server.process(handler, request, response);
      THEN("a method not found error should be returned") {
        REQUIRE(response.m_data.empty() == true);
      }
    }
  }
}
