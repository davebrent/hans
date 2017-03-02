#ifndef HANS_INTERPRETER_H_
#define HANS_INTERPRETER_H_

#include <stddef.h>
#include <vector>
#include "hans/primitives.hpp"

namespace hans {
namespace interpreter {

struct Integer {
  static const uint32_t max = 0xFFFFFFF;
  static uint32_t make(int32_t number);
  static int32_t get(uint32_t integer);
  static bool predicate(uint32_t integer);
};

struct Float {
  static const uint32_t max = 0x4000;
  static uint32_t make(float number);
  static float get(uint32_t integer);
  static bool predicate(uint32_t integer);
};

struct Instruction {
  static const uint32_t max = 0xFFFFFFF;
  static uint32_t make(int32_t number);
  static bool predicate(uint32_t value);
};

struct Pair {
  static const uint32_t max = 0x7FFE;
  static uint32_t make(uint32_t head, uint32_t tail);
  static uint32_t head(uint32_t pair);
  static uint32_t tail(uint32_t pair);
  static bool predicate(uint32_t integer);
};

enum Code {
  PUSH,
  BEGIN,
  END,
  ADD,
  REST,
  DURATION,
  CALL,
  REPEAT,
  REVERSE,
  EVERY,
  SHUFFLE,
  ROTATE,
  DEGRADE,
  CYCLE,
  PALINDROME,
};

using IStack = std::vector<uint32_t>;
using DStack = std::vector<uint32_t>;

IStack compile(std::istream& is);
EventList to_events(const Cycle& cycle, const std::vector<uint32_t>& heap,
                    const uint32_t root);

} // namespace interpreter

struct Interpreter {
  Cycle& cycle;
  interpreter::DStack dstack;
  interpreter::IStack istack;
  std::vector<uint32_t> heap;
  Interpreter(Cycle& cycle);
  Interpreter(Cycle& cycle, interpreter::IStack istack);
};

void interpret(Interpreter& interpreter);

} // namespace hans

#endif // HANS_INTERPRETER_H_
