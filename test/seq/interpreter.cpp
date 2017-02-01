#include "hans/seq/interpreter.hpp"
#include <catch.hpp>
#include <iostream>
#include "hans/seq/primitives.hpp"

using namespace hans::seq;

TEST_CASE("Stack interpreter", "[interpreter]") {
  SECTION("Compiling/lexing?") {
    std::istringstream ss("[ 12 2 3 ] [ 2 3 ] 50 degrade");
    auto tokens = hans::seq::compile(ss);
    IStack expected = {BEGIN, 12, 2, 3, END, BEGIN, 2, 3, END, 50, DEGRADE};
    REQUIRE(tokens == expected);
  }

  SECTION("Compiling/lexing with newlines") {
    std::istringstream ss("[ 12 2 3 ] \n [ 2 3 ] 50 degrade");
    auto tokens = hans::seq::compile(ss);
    IStack expected = {BEGIN, 12, 2, 3, END, BEGIN, 2, 3, END, 50, DEGRADE};
    REQUIRE(tokens == expected);
  }

  SECTION("BEGIN and END codes") {
    // [ 1 . . [ 2 . 3 ] ]
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, REST, BEGIN, 2, REST, 3, END, END});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);

    auto tree = value.tree;
    REQUIRE(tree.children.size() == 4);
    REQUIRE(tree.children.at(0).value == 1);
    REQUIRE(tree.children.at(3).children.size() == 3);
    REQUIRE(tree.children.at(3).children.at(0).value == 2);
    REQUIRE(tree.children.at(3).children.at(2).value == 3);
  }

  SECTION("ADD codes") {
    // [ 1 . . ] [2 . ] add
    Cycle cycle;
    Interpreter itp(cycle,
                    {BEGIN, 1, REST, REST, END, BEGIN, 2, REST, END, ADD});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);

    auto tree = value.tree;
    REQUIRE(tree.children.size() == 2);
    REQUIRE(tree.children.at(0).children.size() == 3);
    REQUIRE(tree.children.at(0).children.at(0).value == 1);

    REQUIRE(tree.children.at(1).children.size() == 2);
    REQUIRE(tree.children.at(1).children.at(0).value == 2);
  }

  SECTION("REF & CALL codes") {
    // [ 1 . . ] [2 . ] & add call
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, REST, END, BEGIN, 2, REST, END, REF,
                            ADD, CALL});

    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);

    auto tree = value.tree;
    REQUIRE(tree.children.size() == 2);
    REQUIRE(tree.children.at(0).children.size() == 3);
    REQUIRE(tree.children.at(0).children.at(0).value == 1);

    REQUIRE(tree.children.at(1).children.size() == 2);
    REQUIRE(tree.children.at(1).children.at(0).value == 2);
  }

  SECTION("REPEAT code") {
    // [ 1 . ] 3 repeat
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, END, 3, REPEAT});

    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);

    auto tree = value.tree;
    REQUIRE(tree.children.size() == 3);
    REQUIRE(tree.children.at(0).children.size() == 2);
    REQUIRE(tree.children.at(0).children.at(0).value == 1);

    REQUIRE(tree.children.at(1).children.size() == 2);
    REQUIRE(tree.children.at(1).children.at(0).value == 1);

    REQUIRE(tree.children.at(2).children.size() == 2);
    REQUIRE(tree.children.at(2).children.at(0).value == 1);
  }

  SECTION("EVERY code") {
    // [ 1 ] 3 & repeat [ . ] 4 every
    Cycle cycle;
    Interpreter itp(
        cycle, {BEGIN, 1, END, 3, REF, REPEAT, BEGIN, REST, END, 4, EVERY});

    itp.cycle.number = 4;

    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);
    REQUIRE(value.tree.children.size() == 3);
    REQUIRE(value.tree.children.at(0).children.size() == 1);
    REQUIRE(value.tree.children.at(0).children.at(0).value == 1);
    REQUIRE(value.tree.children.at(1).children.at(0).value == 1);
    REQUIRE(value.tree.children.at(2).children.at(0).value == 1);
  }

  SECTION("REVERSE code") {
    // [ 1 . . ] reverse
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, REST, REST, END, REVERSE});

    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);
    REQUIRE(value.tree.children.size() == 3);
    REQUIRE(value.tree.children.at(2).value == 1);
    REQUIRE(value.tree.children.at(1).value != 1);
    REQUIRE(value.tree.children.at(0).value != 1);
  }

  SECTION("ROTATE code") {
    // [ 1 2 3 ] 4 rotate
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, 3, END, 4, ROTATE});

    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);
    REQUIRE(value.tree.children.size() == 3);
    REQUIRE(value.tree.children.at(0).value == 2);
    REQUIRE(value.tree.children.at(1).value == 3);
    REQUIRE(value.tree.children.at(2).value == 1);
  }

  SECTION("DURATION code") {
    // 12 dur
    Cycle cycle;
    Interpreter itp(cycle, {12, DURATION});
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 0);
    REQUIRE(itp.cycle.duration.load() == 12);
  }

  SECTION("CYCLE code") {
    // [ 1 2 ] cycle
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, END, CYCLE});
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);
    REQUIRE(value.tree.value == 2);
  }

  SECTION("PALINDROME code") {
    // [ 1 2 ] palindrome
    Cycle cycle;
    Interpreter itp(cycle, {BEGIN, 1, 2, END, PALINDROME});
    itp.cycle.number = 1;
    interpret(itp);
    REQUIRE(itp.dstack.pointer == 1);

    auto value = itp.dstack.pop();
    REQUIRE(value.type == Value::TREE);
    REQUIRE(value.tree.children.size() == 2);
    REQUIRE(value.tree.children.at(0).value == 2);
    REQUIRE(value.tree.children.at(1).value == 1);
  }
}
