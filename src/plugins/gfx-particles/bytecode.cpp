#include "bytecode.hpp"
#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include "hans/hasher.hpp"

using namespace bytecode;

void bytecode::tokenize(std::vector<std::string>& tokens,
                        std::istream& stream) {
  std::istreambuf_iterator<char> it(stream);
  std::istreambuf_iterator<char> end;
  std::vector<char> buff;
  while (it != end) {
    switch (*it) {
    case '\r':
    case '\t':
    case '\n':
    case ' ':
      // Whitespace
      it++;
      break;
    case ';':
      // Comments
      while (*it != '\n' && it != end) {
        it++;
      }
      break;
    case '"':
    case '\'':
      // String literals
      buff.push_back(*it++);
      while (*it != buff.front()) {
        buff.push_back(*it++);
      }
      buff.push_back(*it++);
      tokens.push_back(std::string(buff.begin(), buff.end()));
      buff.clear();
      break;
    default:
      // Regular tokens
      while (*it != ' ' && *it != '\n' && it != end) {
        buff.push_back(*it++);
      }
      tokens.push_back(std::string(buff.begin(), buff.end()));
      buff.clear();
      break;
    }
  }
}

template <typename T>
static bool is_number(const std::string& string) {
  T number;
  std::istringstream iss(string);
  iss >> std::noskipws >> number;
  return iss.eof() && !iss.fail();
}

static uint64_t to_symbol(Program& program, const std::string& token) {
  auto& debug = program.debug;
  auto symbol = hans::hasher(token.c_str());
  auto it = std::find(debug.symbols.begin(), debug.symbols.end(), symbol);
  if (it != debug.symbols.end()) {
    return symbol;
  }
  auto offset = debug.strings.size();
  for (const auto c : token) {
    debug.strings.push_back(c);
  }
  debug.strings.push_back('\0');
  debug.offsets.push_back(offset);
  debug.symbols.push_back(symbol);
  return symbol;
}

static std::string from_symbol(const Program& program, uint64_t symbol) {
  auto& debug = program.debug;
  for (auto it = debug.symbols.begin(); it != debug.symbols.end(); it++) {
    if (*it == symbol) {
      auto i = it - debug.symbols.begin();
      auto offset = debug.offsets.at(i);
      return std::string(&debug.strings.at(offset));
    }
  }
  return std::to_string(symbol);
}

static bool compile_integer(Program& program, const std::string& token) {
  if (!is_number<Integer>(token)) {
    return false;
  }
  auto value = std::stoll(token);
  program.instructions.push_back(LOAD_INTEGER);
  encode<Integer>(program.instructions, value);
  return true;
}

static bool compile_real(Program& program, const std::string& token) {
  if (!is_number<Real>(token)) {
    return false;
  }
  auto value = std::stod(token);
  program.instructions.push_back(LOAD_REAL);
  encode<Real>(program.instructions, value);
  return true;
}

static bool compile_label(Program& program, const std::string& token) {
  if (token.back() != ':') {
    return false;
  }
  auto label = std::string(token.begin(), token.end() - 1);
  auto symbol = to_symbol(program, label);
  program.labels.push_back(symbol);
  auto length = program.instructions.size();
  if (program.sections.size() != 0) {
    program.sections.back().end = length;
  }
  Program::Range range;
  range.start = length;
  program.sections.push_back(range);
  return true;
}

static bool compile_keyword(bytecode::Program& program,
                            const std::string& token) {
  auto it = std::find(Keywords.begin(), Keywords.end(), token);
  if (it == Keywords.end()) {
    return false;
  }
  uint8_t instruction = it - Keywords.begin();
  program.instructions.push_back(instruction);
  return true;
}

static bool compile_symbol(bytecode::Program& program,
                           const std::string& token) {
  if (token.front() != '$') {
    return false;
  }
  auto symbol = to_symbol(program, std::string(token.begin() + 1, token.end()));
  program.instructions.push_back(LOAD_SYMBOL);
  encode<UInteger>(program.instructions, symbol);
  return true;
}

static bool compile_string(bytecode::Program& program,
                           const std::string& token) {
  if ((token.front() != '\'' && token.back() != '\'') &&
      (token.front() != '"' && token.back() != '"')) {
    return false;
  }
  program.instructions.push_back(LOAD_STRING);
  encode<UInteger>(program.instructions, token.size() - 2);
  for (const auto c : std::string(token.begin() + 1, token.end() - 1)) {
    program.instructions.push_back(c);
  }
  program.instructions.push_back('\0');
  return true;
}

static bool compile_variable(bytecode::Program& program,
                             const std::string& token) {
  if (token.front() == '=') {
    auto sym = to_symbol(program, std::string(token.begin() + 1, token.end()));
    program.instructions.push_back(STORE_VAR);
    encode<UInteger>(program.instructions, sym);
    return true;
  } else if (token.front() == '@') {
    auto sym = to_symbol(program, std::string(token.begin() + 1, token.end()));
    program.instructions.push_back(LOAD_VAR);
    encode<UInteger>(program.instructions, sym);
    return true;
  }
  return false;
}

static const std::function<bool(Program&, const std::string&)> compilers[]{
    compile_integer, compile_real,   compile_label,   compile_keyword,
    compile_symbol,  compile_string, compile_variable};

bool bytecode::compile(Program& program,
                       const std::vector<std::string>& tokens) {
  for (const auto& token : tokens) {
    for (const auto& compile : compilers) {
      if (compile(program, token)) {
        break;
      }
    }
  }
  if (program.sections.size()) {
    program.sections.back().end = program.instructions.size();
  }
  return true;
}

void bytecode::decompile(std::ostream& stream, const Program& program) {
  for (auto it = program.sections.begin(); it != program.sections.end(); it++) {
    auto index = it - program.sections.begin();
    auto range = *it;
    std::vector<char> stack(program.instructions.begin() + range.start,
                            program.instructions.begin() + range.end);
    std::reverse(stack.begin(), stack.end());

    auto label = from_symbol(program, program.labels.at(index));
    auto tab = 2;

    stream << label << "(" << (range.end - range.start) << ")"
           << ":" << std::endl;
    while (!stack.empty()) {
      auto code = stack.back();
      stack.pop_back();

      for (auto i = 0; i < tab; ++i) {
        stream << " ";
      }
      stream << Keywords.at(code);

      switch (code) {
      case LOAD_INTEGER:
        stream << " " << fetch<Integer>(stack);
        break;
      case STORE_VAR:
      case LOAD_VAR:
      case LOAD_SYMBOL:
        stream << " " << from_symbol(program, fetch<UInteger>(stack));
        break;
      case LOAD_REAL:
        stream << " " << fetch<Real>(stack);
        break;
      case LOAD_STRING: {
        auto len = fetch<UInteger>(stack);
        stream << " " << len << " \"";
        while (len--) {
          stream << stack.back();
          stack.pop_back();
        }
        stream << "\"";
        stack.pop_back(); // '\0'
        break;
      }
      case LIST_BEGIN:
        tab += 2;
        break;
      default:
        stream << "";
        break;
      }

      if (stack.back() == LIST_END) {
        tab -= 2;
      }
      stream << std::endl;
    }
  }
}

void bytecode::decode(std::ostream& stream, const Program& program,
                      const std::vector<char>& stack) {
  auto code = stack.back();
  if (code >= Keywords.size()) {
    stream << "UNKNOWN" << std::endl;
    return;
  }

  stream << Keywords.at(code);
  switch (code) {
  case LOAD_INTEGER:
    stream << " " << fetch_peek<Integer>(stack, 1);
    break;
  case STORE_VAR:
  case LOAD_VAR:
  case LOAD_SYMBOL:
    stream << " " << from_symbol(program, fetch_peek<UInteger>(stack, 1));
    break;
  case LOAD_REAL:
    stream << " " << fetch_peek<Real>(stack, 1);
    break;
  default:
    stream << "";
    break;
  }

  stream << std::endl;
}
