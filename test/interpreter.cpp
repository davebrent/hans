#include "hans/interpreter.hpp"
#include <catch.hpp>
#include <iostream>
#include "hans/primitives.hpp"

using namespace hans;
using namespace hans::interpreter;

void print(const Value& value) {
  switch (value.type) {
  case Value::LIST:
    std::cout << "LIST (" << value.list.start << ", " << value.list.end << ")";
    break;
  case Value::NUMBER:
    std::cout << "NUMBER (" << value.number << ")";
    break;
  }
}

void print(const std::vector<Value>& values) {
  int index = 0;
  for (const auto& value : values) {
    std::cout << "[" << index << "] ";
    index++;
    print(value);
    std::cout << std::endl;
  }
}

static std::vector<Value> flatten(Interpreter& itp) {
  std::vector<Value> output;
  std::deque<Value> visit;
  visit.push_back(itp.dstack.pop());

  while (!visit.empty()) {
    auto value = visit.front();
    visit.pop_front();

    output.push_back(value);
    if (value.type == Value::LIST) {
      auto begin = itp.heap.begin();
      for (auto it = begin + value.list.end - 1;
           it != begin + value.list.start - 1; --it) {
        visit.push_front(*it);
      }
    }
  }

  return output;
}

static bool equals(const Value& a, const Value& b) {
  if (a.type != b.type) {
    return false;
  }

  switch (a.type) {
  case Value::LIST:
    return a.list.start == b.list.start && a.list.end == b.list.end;
  case Value::NUMBER:
    return a.number == b.number;
  }

  return false;
}

static bool equals(const std::vector<Value>& a, const std::vector<Value>& b) {
  if (a.size() != b.size()) {
    return false;
  }

  for (auto i = 0; i < a.size(); ++i) {
    if (!equals(a.at(i), b.at(i))) {
      return false;
    }
  }

  return true;
}

TEST_CASE("Stack interpreter", "[interpreter]") {
  SECTION("Compiling/lexing?") {
    std::istringstream ss("[ 12 2 3 ] [ 2 3 ] 50 degrade");
    auto tokens = compile(ss);
    IStack expected = {BEGIN, 12, 2, 3, END, BEGIN, 2, 3, END, 50, DEGRADE};
    REQUIRE(tokens == expected);
  }

  SECTION("Compiling/lexing with newlines") {
    std::istringstream ss("[ 12 2 3 ] \n [ 2 3 ] 50 degrade");
    auto tokens = compile(ss);
    IStack expected = {BEGIN, 12, 2, 3, END, BEGIN, 2, 3, END, 50, DEGRADE};
    REQUIRE(tokens == expected);
  }

  SECTION("BEGIN and END codes") {
    // [ 1 . . [ 2 . 3 ] ]
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, REST, BEGIN, 2, REST, 3, END, END});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    // clang-format off
    auto expected = {
      Value(List(3, 7)),
      Value(1),
      Value(REST),
      Value(REST),
      Value(List(0, 3)),
      Value(2),
      Value(REST),
      Value(3)
    };
    // clang-format on
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("ADD codes") {
    // 1 2 add
    Cycle cycle;
    Interpreter itp(cycle, {1, 2, ADD});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    auto expected = {Value(3)};
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("REF & CALL codes") {
    // 1 2 & add call
    Cycle cycle;
    Interpreter itp(cycle, {1, 2, REF, ADD, CALL});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    auto expected = {Value(3)};
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("REPEAT code") {
    // [ 1 . ] 3 repeat
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, END, 3, REPEAT});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    // clang-format off
    auto expected = {
      Value(List(2, 5)),
      Value(List(0, 2)),
      Value(1),
      Value(REST),
      Value(List(0, 2)),
      Value(1),
      Value(REST),
      Value(List(0, 2)),
      Value(1),
      Value(REST),
    };
    // clang-format on
    auto actual = flatten(itp);
    REQUIRE(equals(expected, actual));
  }

  SECTION("EVERY code") {
    // [ 1 ] 3 & repeat [ . ] 4 every
    Cycle cycle;
    cycle.number = 4;
    // clang-format off
    Interpreter itp(cycle, {
      BEGIN, 1, END, 3, REF, REPEAT, BEGIN, REST, END, 4, EVERY
    });
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    auto expected = {
      Value(List(2, 5)),
      Value(List(0, 1)),
      Value(1),
      Value(List(0, 1)),
      Value(1),
      Value(List(0, 1)),
      Value(1)
    };
    // clang-format on
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("REVERSE code") {
    // [ 1 . . ] reverse
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, REST, END, REVERSE});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    // clang-format off
    auto expected = {
      Value(List(0, 3)),
      Value(REST),
      Value(REST),
      Value(1),
    };
    // clang-format on
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("ROTATE code") {
    // [ 1 2 3 ] 4 rotate
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, 3, END, 4, ROTATE});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    // clang-format off
    auto expected = {
      Value(List(0, 3)),
      Value(2),
      Value(3),
      Value(1),
    };
    // clang-format on
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("DURATION code") {
    // 12 dur
    Cycle cycle;
    Interpreter itp(cycle, {12, DURATION});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 0);
    REQUIRE(itp.cycle.duration == 12);
  }

  SECTION("CYCLE code") {
    // [ 1 2 ] cycle
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, END, CYCLE});
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    auto expected = {Value(2)};
    REQUIRE(equals(expected, flatten(itp)));
  }

  SECTION("PALINDROME code") {
    // [ 1 2 ] palindrome
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, END, PALINDROME});
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);
    // clang-format off
    auto expected = {
      Value(List(0, 2)),
      Value(2),
      Value(1),
    };
    // clang-format on
    REQUIRE(equals(expected, flatten(itp)));
  }
}
