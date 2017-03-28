#include "virtual_machine.hpp"
#include <stdlib.h>
#define EIGEN_NO_MALLOC 1
#include <Eigen/Dense>
#include <algorithm>
#include <iostream>

using namespace vm;

void vm::print_buffer(std::ostream& stream, const vm::Buffer& buffer) {
  stream << "buffer id=" << buffer.id << " size=" << buffer.size
         << " channels=" << buffer.channels << std::endl;
  auto columns = 0;
  for (auto i = 0; i < buffer.channels; ++i) {
    stream << "\tchannel=" << i << " components=" << (int)buffer.components[i]
           << std::endl;
    columns += buffer.components[i];
  }

  for (auto i = 0; i < columns * buffer.size; ++i) {
    stream << "\t[" << i << "]=" << buffer.data[i] << std::endl;
  }
}

void vm::print_stack(std::ostream& stream, const std::vector<Value>& stack) {
  for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
    auto value = *it;
    stream << "[" << (it - stack.rbegin()) << "] ";
    switch (value.type) {
    case Value::INTEGER:
      stream << "INTEGER " << value.integer;
      break;
    case Value::REAL:
      stream << "REAL " << value.real;
      break;
    case Value::PAIR:
      stream << "PAIR " << value.pair[0] << " " << value.pair[1];
      break;
    case Value::OPCODE:
      stream << "OPCODE " << value.opcode;
      break;
    case Value::BUFFER:
      stream << "BUFFER size=" << value.buffer.size
             << " channels=" << (int)value.buffer.channels;
      break;
    case Value::POINTER:
      stream << "POINTER " << value.pointer;
      break;
    }
    stream << std::endl;
  }
}

static void assert_types(std::vector<Value>& stack, const std::string& pattern,
                         const std::string& label) {
  auto err = false;
  auto back = 0;
  for (const auto expected : pattern) {
    auto value = *(stack.end() - 1 - back);
    back++;
    switch (expected) {
    case 'i':
      if (value.type != Value::INTEGER) {
        err = true;
      }
      break;
    case 'r':
      if (value.type != Value::REAL) {
        err = true;
      }
      break;
    case 'p':
      if (value.type != Value::PAIR) {
        err = true;
      }
      break;
    case 'o':
      if (value.type != Value::OPCODE) {
        err = true;
      }
      break;
    case 'b':
      if (value.type != Value::BUFFER) {
        err = true;
      }
      break;
    case '*':
      if (value.type != Value::POINTER) {
        err = true;
      }
      break;
    }
    if (err) {
      break;
    }
  }
  if (err) {
    std::cerr << "FUNCTION: " << label << std::endl;
    std::cerr << "\tstack size=" << stack.size() << std::endl;
    print_stack(std::cerr, stack);
    throw std::runtime_error("[HANS] Unexpected value type");
  }
}

static void load_integer(State& state) {
  Value value;
  value.type = Value::INTEGER;
  value.integer = bytecode::fetch<bytecode::Integer>(state.istack);
  state.dstack.push_back(value);
}

static void load_real(State& state) {
  Value value;
  value.type = Value::REAL;
  value.real = bytecode::fetch<bytecode::Real>(state.istack);
  state.dstack.push_back(value);
}

static void load_symbol(State& state) {
  auto symbol = bytecode::fetch<bytecode::UInteger>(state.istack);
  Value value;
  value.type = Value::INTEGER;
  value.integer = symbol;
  state.dstack.push_back(value);
}

static void load_variable(State& state) {
  auto name = bytecode::fetch<bytecode::UInteger>(state.istack);
  auto it = std::find_if(state.variables.begin(), state.variables.end(),
                         [&](const auto& var) { return var.name == name; });
  if (it == state.variables.end()) {
    throw std::runtime_error("[HANS] Unknown variable");
  }

  state.dstack.push_back(state.heap.at((*it).index));
}

static void store_variable(State& state) {
  auto value = state.dstack.back();
  state.dstack.pop_back();
  Variable variable;
  variable.name = bytecode::fetch<bytecode::UInteger>(state.istack);
  variable.index = state.heap.size();
  state.variables.push_back(variable);
  state.heap.push_back(value);
}

static void list_begin(State& state) {
  Value value;
  value.type = Value::OPCODE;
  value.opcode = bytecode::LIST_BEGIN;
  state.dstack.push_back(value);
}

static void list_end(State& state) {
  auto start = state.heap.size();

  while (state.dstack.size()) {
    auto value = state.dstack.back();
    state.dstack.pop_back();

    if (value.type == Value::OPCODE && value.opcode == bytecode::LIST_BEGIN) {
      auto begin = state.heap.begin();
      auto end = state.heap.size();
      std::reverse(begin + start, begin + end);
      Value pair;
      pair.type = Value::PAIR;
      pair.pair[0] = start;
      pair.pair[1] = end;
      state.dstack.push_back(pair);
      return;
    }

    state.heap.push_back(value);
  }

  throw std::runtime_error("[HANS] list_end: Data stack exhausted");
}

static void make_buffer(State& state) {
  assert_types(state.dstack, "ii", "buffer");
  auto size = state.dstack.back();
  state.dstack.pop_back();
  auto channels = state.dstack.back();
  state.dstack.pop_back();

  Buffer buffer;
  buffer.id = state.buffer_ids++;
  buffer.size = size.integer;
  buffer.channels = channels.integer;

  auto samples = 0;
  while (channels.integer--) {
    assert_types(state.dstack, "i", "buffer::channel");
    auto components = state.dstack.back();
    state.dstack.pop_back();

    buffer.components[channels.integer] = components.integer;
    samples += buffer.size * components.integer;
  }

  // XXX: Eigen needs this memory alligned to 128?
  buffer.data = static_cast<Buffer::Value*>(
      aligned_alloc(128, sizeof(Buffer::Value) * samples));
  std::memset(buffer.data, 0, sizeof(Buffer::Value) * samples);
  Value value;
  value.type = Value::BUFFER;
  value.buffer = buffer;
  state.dstack.push_back(value);
}

static uint64_t to_column(Buffer& buffer, uint64_t channel) {
  uint64_t column = 0;
  for (auto i = 0; i < channel; ++i) {
    column += buffer.components[i];
  }
  return column;
}

static void buffer_fill(State& state) {
  assert_types(state.dstack, "ipb", "fill");
  auto channel = state.dstack.back().integer;
  state.dstack.pop_back();

  auto constant = state.dstack.back().pair;
  state.dstack.pop_back();

  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  Buffer::Value vals[MAX_CHANNELS];
  if ((constant[1] - constant[0]) >= MAX_CHANNELS) {
    throw std::runtime_error("[HANS] Max channels exceeded");
  }

  for (auto it = state.heap.begin() + constant[0];
       it < state.heap.begin() + constant[1]; it++) {
    auto index = it - (state.heap.begin() + constant[0]);
    auto value = *it;
    switch (value.type) {
    case Value::INTEGER:
      vals[index] = value.integer;
      break;
    case Value::REAL:
      vals[index] = value.real;
      break;
    default:
      throw std::runtime_error("[HANS] Unsupported fill type");
    }
  }

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);

  auto column = to_column(buffer, channel);
  for (auto i = 0; i < constant[1] - constant[0]; ++i) {
    auto value = vals[i];
    auto block = matrix.block(0, column + i, buffer.size, 1);
    block = Buffer::Matrix::Constant(buffer.size, 1, value);
  }
}

static void buffer_add(State& state) {
  assert_types(state.dstack, "iiib", "add");
  auto to = state.dstack.back().integer;
  state.dstack.pop_back();
  auto rhs = state.dstack.back().integer;
  state.dstack.pop_back();
  auto lhs = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto rhb = matrix.block(0, to_column(buffer, rhs), buffer.size,
                          buffer.components[rhs]);
  auto lhb = matrix.block(0, to_column(buffer, lhs), buffer.size,
                          buffer.components[lhs]);
  auto tob = matrix.block(0, to_column(buffer, to), buffer.size,
                          buffer.components[to]);
  tob = lhb + rhb;
}

static void buffer_sub(State& state) {
  assert_types(state.dstack, "iiib", "sub");
  auto to = state.dstack.back().integer;
  state.dstack.pop_back();
  auto rhs = state.dstack.back().integer;
  state.dstack.pop_back();
  auto lhs = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto rhb = matrix.block(0, to_column(buffer, rhs), buffer.size,
                          buffer.components[rhs]);
  auto lhb = matrix.block(0, to_column(buffer, lhs), buffer.size,
                          buffer.components[lhs]);
  auto tob = matrix.block(0, to_column(buffer, to), buffer.size,
                          buffer.components[to]);
  tob = lhb - rhb;
}

static void buffer_mul(State& state) {
  assert_types(state.dstack, "iiib", "mul");
  auto to = state.dstack.back().integer;
  state.dstack.pop_back();
  auto rhs = state.dstack.back().integer;
  state.dstack.pop_back();
  auto lhs = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto rhb = matrix.block(0, to_column(buffer, rhs), buffer.size,
                          buffer.components[rhs]);
  auto lhb = matrix.block(0, to_column(buffer, lhs), buffer.size,
                          buffer.components[lhs]);
  auto tob = matrix.block(0, to_column(buffer, to), buffer.size,
                          buffer.components[to]);
  tob = lhb.array() * rhb.array();
}

static void buffer_div(State& state) {
  assert_types(state.dstack, "iiib", "div");
  auto to = state.dstack.back().integer;
  state.dstack.pop_back();
  auto rhs = state.dstack.back().integer;
  state.dstack.pop_back();
  auto lhs = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto rhb = matrix.block(0, to_column(buffer, rhs), buffer.size,
                          buffer.components[rhs]);
  auto lhb = matrix.block(0, to_column(buffer, lhs), buffer.size,
                          buffer.components[lhs]);
  auto tob = matrix.block(0, to_column(buffer, to), buffer.size,
                          buffer.components[to]);
  tob = lhb.array() / rhb.array();
}

static void random_noise(State& state) {
  assert_types(state.dstack, "ib", "random-noise");
  auto channel = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto block = matrix.block(0, to_column(buffer, channel), buffer.size,
                            buffer.components[channel]);
  block.setRandom();
}

static void distribute_linear(State& state) {
  assert_types(state.dstack, "rriib", "distribute-linear");
  auto to = state.dstack.back().real;
  state.dstack.pop_back();
  auto from = state.dstack.back().real;
  state.dstack.pop_back();
  auto component = state.dstack.back().integer;
  state.dstack.pop_back();
  auto channel = state.dstack.back().integer;
  state.dstack.pop_back();
  // Leave the buffer on the stack
  auto buffer = state.dstack.back().buffer;

  auto cols = to_column(buffer, buffer.channels);
  auto matrix = Eigen::Map<Buffer::Matrix>(buffer.data, buffer.size, cols);
  auto block = matrix.block(0, to_column(buffer, channel), buffer.size,
                            buffer.components[channel]);
  block.col(component).setLinSpaced(block.rows(), from, to);
}

static void render(State& state) {
  assert_types(state.dstack, "ipb", "render");
  auto name = state.dstack.back().integer;
  state.dstack.pop_back();
  auto opts = state.dstack.back().pair;
  state.dstack.pop_back();
  auto buffer = state.dstack.back().buffer;
  state.dstack.pop_back();

  switch (name) {
  case VISUALISER_POINTS: {
    assert((opts[1] - opts[0]) % 2 == 0);

    visualisers::PointsVisualiser::Options options;
    for (auto it = state.heap.begin() + opts[0];
         it < state.heap.begin() + opts[1]; it += 2) {
      auto key = *it;
      auto val = *(it + 1);
      assert(key.type == Value::INTEGER);
      assert(val.type == Value::INTEGER);

      switch (key.integer) {
      case visualisers::PointsVisualiser::POSITION:
        options.position = val.integer;
        break;
      case visualisers::PointsVisualiser::COLOR:
        options.color = val.integer;
        break;
      }
    }

    state.visualiser_points.set(buffer, options);
    break;
  }
  default:
    throw std::runtime_error("[HANS] Unknown visualiser");
    break;
  }

  state.draw_cmds.push_back(name);
}

void vm::eval(State& state, const bytecode::Program& program, uint64_t label) {
  auto it = std::find(program.labels.begin(), program.labels.end(), label);
  if (it == program.labels.end()) {
    return;
  }

  // Setup the instruction stack for this sections code
  auto range = program.sections.at(it - program.labels.begin());
  state.istack.clear();
  state.istack.reserve(range.end - range.start);
  for (auto i = range.start; i < range.end; ++i) {
    state.istack.push_back(program.instructions.at(i));
  }
  std::reverse(state.istack.begin(), state.istack.end());

  // XXX: Needs some more thought
  std::srand(state.seed);
  state.draw_cmds.clear();

  // Clear everything in the heap above a certain point
  if (state.heap_frame_start != -1) {
    state.heap.erase(state.heap.begin() + state.heap_frame_start,
                     state.heap.end());
  }

  while (!state.istack.empty()) {
    auto code = state.istack.back();
    state.istack.pop_back();

    switch (code) {
    case bytecode::LOAD_INTEGER:
      load_integer(state);
      break;
    case bytecode::LOAD_REAL:
      load_real(state);
      break;
    case bytecode::LOAD_SYMBOL:
      load_symbol(state);
      break;
    case bytecode::LOAD_VAR:
      load_variable(state);
      break;
    case bytecode::STORE_VAR:
      store_variable(state);
      break;
    case bytecode::LIST_BEGIN:
      list_begin(state);
      break;
    case bytecode::LIST_END:
      list_end(state);
      break;
    case bytecode::BUFFER:
      make_buffer(state);
      break;
    case bytecode::BUFFER_FILL:
      buffer_fill(state);
      break;
    case bytecode::BUFFER_ADD:
      buffer_add(state);
      break;
    case bytecode::BUFFER_SUB:
      buffer_sub(state);
      break;
    case bytecode::BUFFER_MUL:
      buffer_mul(state);
      break;
    case bytecode::BUFFER_DIV:
      buffer_div(state);
      break;
    case bytecode::RANDOM_NOISE:
      random_noise(state);
      break;
    case bytecode::RANDOM_SPHERE:
      break;
    case bytecode::DISTRIBUTE_LINEAR:
      distribute_linear(state);
      break;
    case bytecode::RENDER:
      render(state);
      break;
    }
  }
}
