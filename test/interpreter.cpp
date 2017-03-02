#include "hans/interpreter.hpp"
#include <catch.hpp>
#include <iostream>
#include "hans/primitives.hpp"

using namespace hans;
using namespace hans::interpreter;

void print(const uint32_t value) {
  if (Pair::predicate(value)) {
    std::cout << "LIST (" << Pair::head(value);
    std::cout << ", " << Pair::tail(value) << ")";
  } else {
    if (Instruction::predicate(value)) {
      std::cout << "INSTRUCTION (" << Integer::get(value) << ")";
    } else {
      std::cout << "NUMBER (" << Integer::get(value) << ")";
    }
  }
}

void print(const std::vector<uint32_t>& values) {
  int index = 0;
  for (const auto value : values) {
    std::cout << "[" << index << "] ";
    index++;
    print(value);
    std::cout << std::endl;
  }
}

static std::vector<uint32_t> flatten(Interpreter& itp) {
  std::vector<uint32_t> output;
  std::deque<uint32_t> visit;
  visit.push_back(itp.dstack.back());
  itp.dstack.pop_back();

  while (!visit.empty()) {
    auto value = visit.front();
    visit.pop_front();
    output.push_back(value);

    if (Pair::predicate(value)) {
      auto begin = itp.heap.begin();
      auto head = Pair::head(value);
      auto tail = Pair::tail(value);
      for (auto it = begin + tail - 1; it != begin + head - 1; --it) {
        visit.push_front(*it);
      }
    }
  }

  return output;
}

static IStack exp(const std::string& code) {
  std::istringstream ss(code);
  return compile(ss);
}

TEST_CASE("Stack interpreter", "[interpreter]") {
  SECTION("Instruction format") {
    uint32_t negative = Integer::make(-122);
    REQUIRE(Integer::get(negative) == -122);
    REQUIRE(Integer::get(Instruction::make(-122)) == -122);

    uint32_t positive = Integer::make(122);
    REQUIRE(positive == 122);
    REQUIRE(Integer::get(positive) == 122);
    REQUIRE(Integer::get(Instruction::make(positive)) == 122);

    REQUIRE(Instruction::predicate(Instruction::make(100)) == true);
    REQUIRE(Float::get(Float::make(-3210.145)) == Approx(-3210.145));

    REQUIRE(Integer::predicate(Integer::make(20)) == true);
    REQUIRE(Float::predicate(Float::make(-3210.145)) == true);
    REQUIRE(Float::predicate(Integer::make(100)) == false);
  }

  SECTION("Pairs") {
    uint32_t pair = Pair::make(200, 400);
    REQUIRE(Pair::head(pair) == 200);
    REQUIRE(Pair::tail(pair) == 400);
  }

  SECTION("Assembling") {
    auto tokens = exp("[ 12 2 3 ] [ 2 3 ] 50 degrade");
    IStack expected = {
        Instruction::make(DEGRADE),
        50,
        Instruction::make(PUSH),
        Instruction::make(END),
        3,
        Instruction::make(PUSH),
        2,
        Instruction::make(PUSH),
        Instruction::make(BEGIN),
        Instruction::make(END),
        3,
        Instruction::make(PUSH),
        2,
        Instruction::make(PUSH),
        12,
        Instruction::make(PUSH),
        Instruction::make(BEGIN),
    };
    REQUIRE(tokens == expected);
  }

  SECTION("Assembling with newlines") {
    auto tokens = exp("[ 12 2 3 ] \n [ 2 3 ] 50 degrade");
    IStack expected = {
        Instruction::make(DEGRADE),
        50,
        Instruction::make(PUSH),
        Instruction::make(END),
        3,
        Instruction::make(PUSH),
        2,
        Instruction::make(PUSH),
        Instruction::make(BEGIN),
        Instruction::make(END),
        3,
        Instruction::make(PUSH),
        2,
        Instruction::make(PUSH),
        12,
        Instruction::make(PUSH),
        Instruction::make(BEGIN),
    };
    REQUIRE(tokens == expected);
  }

  SECTION("ADD codes") {
    Cycle cycle;
    Interpreter itp(cycle, exp("1 2 add"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{3};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("ADD negative values") {
    Cycle cycle;
    Interpreter itp(cycle, exp("1 -10 add"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    REQUIRE(Integer::get(flatten(itp).at(0)) == -9);
  }

  SECTION("BEGIN and END codes") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 . ]"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);

    std::vector<uint32_t> expected{Pair::make(0, 2), 1,
                                   Instruction::make(REST)};

    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("nested BEGIN and END codes") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 100 [ 2 300 ] ]"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{Pair::make(2, 4), 100, Pair::make(0, 2), 2,
                                   300};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("REF & CALL codes") {
    Cycle cycle;
    Interpreter itp(cycle, exp("1 2 push add call"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{3};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("REPEAT code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 . ] 3 repeat"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{Pair::make(2, 5),        Pair::make(0, 2), 1,
                                   Instruction::make(REST), Pair::make(0, 2), 1,
                                   Instruction::make(REST), Pair::make(0, 2), 1,
                                   Instruction::make(REST)};

    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("EVERY code") {
    Cycle cycle;
    cycle.number = 4;
    Interpreter itp(cycle, exp("[ 1 ] 3 push repeat [ . ] 4 every"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{Pair::make(2, 5),
                                   Pair::make(0, 1),
                                   1,
                                   Pair::make(0, 1),
                                   1,
                                   Pair::make(0, 1),
                                   1};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("REVERSE code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 . . ] reverse"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{
        Pair::make(0, 3), Instruction::make(REST), Instruction::make(REST), 1,
    };
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("ROTATE code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 2 3 ] 4 rotate"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{Pair::make(0, 3), 2, 3, 1};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("DURATION code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("12 dur"));
    interpret(itp);
    REQUIRE(itp.dstack.size() == 0);
    REQUIRE(itp.cycle.duration == 12);
  }

  SECTION("CYCLE code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 2 ] cycle"));
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{2};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }

  SECTION("PALINDROME code") {
    Cycle cycle;
    Interpreter itp(cycle, exp("[ 1 2 ] palindrome"));
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.size() == 1);
    std::vector<uint32_t> expected{Pair::make(0, 2), 2, 1};
    auto actual = flatten(itp);
    REQUIRE(expected == actual);
  }
}
