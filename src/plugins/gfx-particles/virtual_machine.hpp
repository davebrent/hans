#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "buffer.hpp"
#include "bytecode.hpp"
#include "visualisers.hpp"

#define VISUALISER_POINTS 0x2928a7f64e9553ef /* points */

namespace vm {

struct Value {
  enum Type {
    INTEGER,
    REAL,
    PAIR,
    BUFFER,
    OPCODE,
    POINTER,
  };
  Type type;
  union {
    char opcode;
    double real;
    uint64_t pair[2];
    uint64_t integer;
    Buffer buffer;
    uint64_t pointer;
  };
};

struct Variable {
  uint64_t name;
  uint64_t index;
};

struct State {
  std::vector<char> istack;
  std::vector<Value> dstack;
  std::vector<Value> heap;
  std::vector<Variable> variables;
  uint64_t seed;
  uint64_t buffer_ids;
  int64_t heap_frame_start = -1;
  visualisers::PointsVisualiser visualiser_points;
  std::vector<uint64_t> draw_cmds;
};

void print_buffer(std::ostream& stream, const vm::Buffer& buffer);
void print_stack(std::ostream& stream, const std::vector<Value>& stack);
void eval(State& state, const bytecode::Program& program, uint64_t label);

} // namespace vm

#endif // VIRTUAL_MACHINE_H_
