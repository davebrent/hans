#include "bytecode.hpp"
#include <catch.hpp>

TEST_CASE("Bytecode", "[bytecode]") {
  SECTION("Encoding data") {
    std::vector<char> instructions;
    instructions.push_back(bytecode::LOAD_INTEGER);
    bytecode::encode<bytecode::Integer>(instructions, -122);
    instructions.push_back(bytecode::LOAD_REAL);
    bytecode::encode<bytecode::Real>(instructions, 3.1456);

    std::reverse(instructions.begin(), instructions.end());

    REQUIRE(instructions.back() == bytecode::LOAD_INTEGER);
    instructions.pop_back();
    REQUIRE(bytecode::fetch<bytecode::Integer>(instructions) == -122);
    REQUIRE(instructions.back() == bytecode::LOAD_REAL);
    instructions.pop_back();
    REQUIRE(bytecode::fetch<bytecode::Real>(instructions) == 3.1456);
  }
}
