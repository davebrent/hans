#ifndef BYTECODE_H_
#define BYTECODE_H_

#include <cstring>
#include <istream>
#include <ostream>
#include <string>
#include <vector>

namespace bytecode {

using Integer = int64_t;
using UInteger = uint64_t;
using Real = double;

static const std::vector<const char*> Keywords{
    "loadint",
    "loadreal",
    "loadsymbol",
    "loadstring",
    "loadvar",
    "storevar",
    "[",
    "]",
    "buffer",
    "buffer-fill",
    "buffer-add",
    "buffer-sub",
    "buffer-mul",
    "buffer-div",
    "random-noise",
    "random-sphere",
    "distribute-linear",
    "render",
};

enum Opcode {
  LOAD_INTEGER,
  LOAD_REAL,
  LOAD_SYMBOL,
  LOAD_STRING,
  LOAD_VAR,
  STORE_VAR,
  LIST_BEGIN,
  LIST_END,
  BUFFER,
  BUFFER_FILL,
  BUFFER_ADD,
  BUFFER_SUB,
  BUFFER_MUL,
  BUFFER_DIV,
  RANDOM_NOISE,
  RANDOM_SPHERE,
  DISTRIBUTE_LINEAR,
  RENDER,
};

struct Program {
  struct Range {
    uint64_t start;
    uint64_t end;
  };

  struct Debug {
    std::vector<uint64_t> symbols;
    std::vector<uint64_t> offsets;
    std::vector<char> strings;
  };

  std::vector<uint64_t> labels;
  std::vector<Range> sections;
  std::vector<char> instructions;
  Debug debug;
};

void tokenize(std::vector<std::string>& tokens, std::istream& stream);
bool compile(Program& program, const std::vector<std::string>& tokens);
void decompile(std::ostream& stream, const Program& program);
void decode(std::ostream& stream, const Program& program,
            const std::vector<char>& stack);

// Encode a value into the instruction stream
template <typename T>
void encode(std::vector<char>& instructions, T value) {
  char bytes[sizeof(T)];
  std::memcpy(&bytes, &value, sizeof(T));
  for (const auto b : bytes) {
    instructions.push_back(b);
  }
}

// Return a value stored in the instruction stream
template <typename T>
T fetch(std::vector<char>& stack) {
  char bytes[sizeof(T)];
  for (auto i = 0; i < sizeof(T); ++i) {
    bytes[i] = stack.back();
    stack.pop_back();
  }
  T value;
  std::memcpy(&value, &bytes, sizeof(T));
  return value;
}

// Return a value stored in the instruction stream without pop elements off
template <typename T>
T fetch_peek(const std::vector<char>& stack, size_t offset) {
  char bytes[sizeof(T)];
  for (auto i = 0; i < sizeof(T); ++i) {
    bytes[i] = *(stack.end() - 1 - i - offset);
  }
  T value;
  std::memcpy(&value, &bytes, sizeof(T));
  return value;
}

} // namespace bytecode

#endif // BYTECODE_H_
