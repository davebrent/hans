#ifndef HANS_INTERPRETER_H_
#define HANS_INTERPRETER_H_

#include <stddef.h>
#include <deque>
#include <vector>
#include "hans/sequencer.hpp"

namespace hans {
namespace interpreter {

static const auto REST_VALUE = 0xffff;

// clang-format off
enum Code {
  REST       = 0x10000,
  BEGIN      = 0x10001,
  END        = 0x10002,
  ADD        = 0x10003,
  REF        = 0x10004,
  DURATION   = 0x10005,
  CALL       = 0x10006,
  REPEAT     = 0x10007,
  REVERSE    = 0x10008,
  EVERY      = 0x10009,
  SHUFFLE    = 0x1000A,
  ROTATE     = 0x1000B,
  DEGRADE    = 0x1000C,
  CYCLE      = 0x1000D,
  PALINDROME = 0x1000E,
};

static const std::vector<const char*> Words = {
  ".",
  "[",
  "]",
  "add",
  "&",
  "dur",
  "call",
  "repeat",
  "reverse",
  "every",
  "shuffle",
  "rotate",
  "degrade",
  "cycle",
  "palindrome",
};
// clang-format on

struct Tree {
  uint16_t value = 0;
  std::vector<Tree> children;
};

struct Value {
  enum Type { TREE, NUMBER, UNDEFINED };
  Type type;
  Tree tree;
  size_t number;
  Value();
  Value(Tree tree);
  Value(size_t number);
};

using IStack = std::deque<uint32_t>;

class DStack {
 public:
  size_t pointer = 0;
  Value pop();
  Value peek_back();
  void push(Value value);
  bool empty();

 private:
  std::vector<Value> buffer;
};

IStack compile(std::istream& is);
sequencer::EventList to_events(const sequencer::Cycle& cycle, const Tree& tree);

} // namespace interpreter

struct Interpreter {
  sequencer::Cycle& cycle;
  interpreter::DStack dstack;
  interpreter::IStack istack;
  Interpreter(sequencer::Cycle& cycle);
  Interpreter(sequencer::Cycle& cycle, interpreter::IStack istack);
};

void interpret(Interpreter& interpreter);

} // namespace hans

#endif // HANS_INTERPRETER_H_
