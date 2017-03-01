#ifndef HANS_INTERPRETER_H_
#define HANS_INTERPRETER_H_

#include <stddef.h>
#include <deque>
#include <vector>
#include "hans/primitives.hpp"

namespace hans {
namespace interpreter {

// clang-format off
enum Code {
  REST       = 0xFFFF,
  BEGIN      = 0x10000,
  END        = 0x10001,
  ADD        = 0x10002,
  REF        = 0x10003,
  DURATION   = 0x10004,
  CALL       = 0x10005,
  REPEAT     = 0x10006,
  REVERSE    = 0x10007,
  EVERY      = 0x10008,
  SHUFFLE    = 0x10009,
  ROTATE     = 0x1000A,
  DEGRADE    = 0x1000B,
  CYCLE      = 0x1000C,
  PALINDROME = 0x1000D,
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

struct List {
  size_t start;
  size_t end;
  List();
  List(size_t start, size_t end);
};

struct Value {
  enum Type { NUMBER, LIST };
  Type type;
  union {
    uint32_t number;
    List list;
  };

  Value(List list);
  Value(uint32_t number);
};

using IStack = std::deque<uint32_t>;

class DStack {
 public:
  size_t pointer = 0;
  Value pop();
  void push(Value value);
  bool empty();

 private:
  std::vector<Value> buffer;
};

IStack compile(std::istream& is);
EventList to_events(const Cycle& cycle, const std::vector<Value>& heap,
                    const Value& value);

} // namespace interpreter

struct Interpreter {
  Cycle& cycle;
  interpreter::DStack dstack;
  interpreter::IStack istack;
  std::vector<interpreter::Value> heap;
  Interpreter(Cycle& cycle);
  Interpreter(Cycle& cycle, interpreter::IStack istack);
};

void interpret(Interpreter& interpreter);

} // namespace hans

#endif // HANS_INTERPRETER_H_
