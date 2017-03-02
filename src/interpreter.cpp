#include "hans/interpreter.hpp"
#include <algorithm>
#include <functional>
#include <istream>
#include <iterator>
#include <random>
#include <stdexcept>

using namespace hans;
using namespace hans::interpreter;

static uint32_t INSTRUCTION_BIT = 31;
static uint32_t PAIR_BIT = 30;
static uint32_t FLOAT_BIT = 29;
static uint32_t SIGNED_BIT = 28;

Interpreter::Interpreter(Cycle& cycle, IStack istack)
    : cycle(cycle), istack(istack){};

static uint32_t set_bit(uint32_t number, uint32_t bit) {
  return number | (1 << bit);
}

static bool has_bit_set(uint32_t number, uint32_t bit) {
  return number & (1 << bit);
}

uint32_t hans::interpreter::Integer::make(int32_t number) {
  if (abs(number) >= Integer::max) {
    throw std::runtime_error("Integer maximum exceeded");
  }
  if (number < 0) {
    number = ~number + 1;
    number = set_bit(number, SIGNED_BIT);
  }
  return number;
}

int32_t hans::interpreter::Integer::get(uint32_t number) {
  number &= ~(1 << INSTRUCTION_BIT);
  number &= ~(1 << PAIR_BIT);
  number &= ~(1 << FLOAT_BIT);
  if (number & (1 << SIGNED_BIT)) {
    number &= ~(1 << SIGNED_BIT);
    return ~number + 1;
  }
  return number;
}

bool hans::interpreter::Integer::predicate(uint32_t number) {
  return !has_bit_set(number, PAIR_BIT) && !has_bit_set(number, FLOAT_BIT);
}

uint32_t hans::interpreter::Float::make(float number) {
  int32_t whole = std::floor(std::abs(number));
  uint32_t fractional = (std::abs(number) - whole) * Float::max;
  uint32_t output = 0;
  output |= (fractional << 0);
  output |= (whole << 14);
  if (number < 0) {
    output = set_bit(output, SIGNED_BIT);
  }
  return set_bit(output, FLOAT_BIT);
}

float hans::interpreter::Float::get(uint32_t number) {
  uint32_t whole = (0xfffc000 & number) >> 14;
  float fract = ((0x3fff & number) >> 0) / ((float)Float::max);
  float result = whole + fract;
  if (has_bit_set(number, SIGNED_BIT)) {
    return result * -1.f;
  }
  return result;
}

bool hans::interpreter::Float::predicate(uint32_t number) {
  return has_bit_set(number, FLOAT_BIT);
}

uint32_t hans::interpreter::Instruction::make(int32_t number) {
  return set_bit(Integer::make(number), INSTRUCTION_BIT);
}

bool hans::interpreter::Instruction::predicate(uint32_t number) {
  return has_bit_set(number, INSTRUCTION_BIT) && Integer::predicate(number);
}

uint32_t hans::interpreter::Pair::make(uint32_t start, uint32_t end) {
  if (start >= Pair::max || end >= Pair::max) {
    throw std::runtime_error("list index out of range");
  }
  uint32_t pair = 0;
  pair |= (start << 0);
  pair |= (end << 14);
  return set_bit(pair, PAIR_BIT);
}

uint32_t hans::interpreter::Pair::head(uint32_t pair) {
  return (0x3fff & pair) >> 0;
}

uint32_t hans::interpreter::Pair::tail(uint32_t pair) {
  return (0xfffc000 & pair) >> 14;
}

bool hans::interpreter::Pair::predicate(uint32_t number) {
  return has_bit_set(number, PAIR_BIT);
}

static void push(Interpreter& itp, uint32_t code) {
  auto integer = itp.istack.back();
  itp.istack.pop_back();
  itp.dstack.push_back(integer);
}

static void begin(Interpreter& itp, uint32_t code) {
  itp.dstack.push_back(Instruction::make(BEGIN));
}

static void end(Interpreter& itp, uint32_t code) {
  auto start = itp.heap.size();

  while (true) {
    auto value = itp.dstack.back();
    itp.dstack.pop_back();

    if (Instruction::predicate(value) && Integer::get(value) == BEGIN) {
      auto begin = itp.heap.begin();
      auto end = itp.heap.size();
      std::reverse(begin + start, begin + end);
      itp.dstack.push_back(Pair::make(start, end));
      return;
    }

    itp.heap.push_back(value);
  }
}

static void rest(Interpreter& itp, uint32_t code) {
  itp.dstack.push_back(Instruction::make(REST));
}

static void add(Interpreter& itp, uint32_t code) {
  auto rhs = itp.dstack.back();
  itp.dstack.pop_back();

  auto lhs = itp.dstack.back();
  itp.dstack.pop_back();

  auto result = Integer::get(lhs) + Integer::get(rhs);
  itp.dstack.push_back(Integer::make(result));
}

static void duration(Interpreter& itp, uint32_t code) {
  itp.cycle.duration = Integer::get(itp.dstack.back());
  itp.dstack.pop_back();
}

static void call(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.back();
  itp.dstack.pop_back();
  itp.istack.push_back(value);
}

static void repeat(Interpreter& itp, uint32_t code) {
  auto n = Integer::get(itp.dstack.back());
  itp.dstack.pop_back();

  auto value = itp.dstack.back();
  itp.dstack.pop_back();

  auto start = itp.heap.size();
  auto root = Pair::make(start, start + n);

  for (auto i = 0; i < n; ++i) {
    itp.heap.push_back(value);
  }

  itp.dstack.push_back(root);
}

static void every(Interpreter& itp, uint32_t code) {
  // XXX: else clause is letf on the dstack
  auto freq = Integer::get(itp.dstack.back());
  itp.dstack.pop_back();

  if (freq % itp.cycle.number == 0) {
    itp.dstack.pop_back();
    auto value = itp.dstack.back();
    itp.dstack.pop_back();
    itp.istack.push_back(value);
  }
}

static void reverse(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.back();
  itp.dstack.pop_back();

  auto begin = itp.heap.begin();
  std::reverse(begin + Pair::head(value), begin + Pair::tail(value));
  itp.dstack.push_back(value);
}

static void shuffle(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.back();
  itp.dstack.pop_back();

  auto begin = itp.heap.begin();
  std::random_device rd;
  std::mt19937 g(rd());
  std::shuffle(begin + Pair::head(value), begin + Pair::tail(value), g);
  itp.dstack.push_back(value);
}

static void rotate(Interpreter& itp, uint32_t code) {
  auto amount = Integer::get(itp.dstack.back());
  itp.dstack.pop_back();

  auto value = itp.dstack.back();
  itp.dstack.pop_back();

  auto head = Pair::head(value);
  auto tail = Pair::tail(value);

  if (head != tail) {
    amount %= tail - head;
    auto begin = itp.heap.begin();
    std::rotate(begin + head, begin + head + amount, begin + tail);
  }

  itp.dstack.push_back(value);
}

static void degrade(Interpreter& itp, uint32_t code) {
  auto amount = Integer::get(itp.dstack.back());
  itp.dstack.pop_back();

  auto value = itp.dstack.back();
  itp.dstack.pop_back();

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 100);

  auto begin = itp.heap.begin();
  auto head = Pair::head(value);
  auto tail = Pair::tail(value);
  for (auto it = begin + head; it != begin + tail; ++it) {
    if (dis(gen) < amount) {
      *it = set_bit(REST, INSTRUCTION_BIT);
    }
  }

  itp.dstack.push_back(value);
}

static void cycle(Interpreter& itp, uint32_t code) {
  auto list = itp.dstack.back();
  itp.dstack.pop_back();

  auto head = Pair::head(list);
  auto tail = Pair::tail(list);
  if (head != tail) {
    auto i = itp.cycle.number % (tail - head);
    itp.dstack.push_back(itp.heap.at(i));
  }
}

static void palindrome(Interpreter& itp, uint32_t code) {
  if (itp.cycle.number % 2 == 1) {
    reverse(itp, code);
  }
}

static const std::vector<const char*> WORDS = {
    "push",    "[",      "]",       "add",     ".",
    "dur",     "call",   "repeat",  "reverse", "every",
    "shuffle", "rotate", "degrade", "cycle",   "palindrome",
};

static bool assemble_instruction(IStack& istack, const std::string& token) {
  auto it = std::find(WORDS.begin(), WORDS.end(), token);
  if (it != WORDS.end()) {
    auto instruction = (it - WORDS.begin());
    istack.push_back(Instruction::make(instruction));
    return true;
  }
  return false;
}

static bool assemble_float(IStack& istack, const std::string& token) {
  if (token.find(".") != std::string::npos) {
    float value = std::stof(token);
    istack.push_back(Instruction::make(PUSH));
    istack.push_back(Float::make(value));
    return true;
  }
  return false;
}

static bool assemble_integer(IStack& istack, const std::string& token) {
  auto value = std::stol(token);
  istack.push_back(Instruction::make(PUSH));
  istack.push_back(Integer::make(value));
  return true;
}

IStack hans::interpreter::compile(std::istream& is) {
  IStack istack;

  std::vector<std::string> tokens;
  std::copy(std::istream_iterator<std::string>(is),
            std::istream_iterator<std::string>(), std::back_inserter(tokens));

  for (const auto& token : tokens) {
    if (assemble_instruction(istack, token)) {
      continue;
    } else if (assemble_float(istack, token)) {
      continue;
    } else if (assemble_integer(istack, token)) {
      continue;
    } else {
      throw std::runtime_error("hans::interpreter Failed to assembled token");
    }
  }

  std::reverse(istack.begin(), istack.end());
  return istack;
}

void hans::interpret(Interpreter& itp) {
  while (!itp.istack.empty()) {
    auto code = Integer::get(itp.istack.back());
    itp.istack.pop_back();

    switch (code) {
    case PUSH:
      push(itp, code);
      break;
    case BEGIN:
      begin(itp, code);
      break;
    case END:
      end(itp, code);
      break;
    case REST:
      rest(itp, code);
      break;
    case ADD:
      add(itp, code);
      break;
    case DURATION:
      duration(itp, code);
      break;
    case CALL:
      call(itp, code);
      break;
    case REPEAT:
      repeat(itp, code);
      break;
    case EVERY:
      every(itp, code);
      break;
    case REVERSE:
      reverse(itp, code);
      break;
    case SHUFFLE:
      shuffle(itp, code);
      break;
    case ROTATE:
      rotate(itp, code);
      break;
    case DEGRADE:
      degrade(itp, code);
      break;
    case CYCLE:
      cycle(itp, code);
      break;
    case PALINDROME:
      palindrome(itp, code);
      break;
    };
  }
}

struct Time {
  float start;
  float duration;
  Time(float start, float duration) : start(start), duration(duration){};
};

EventList hans::interpreter::to_events(const Cycle& cycle,
                                       const std::vector<uint32_t>& heap,
                                       const uint32_t root) {
  EventList events;
  std::deque<std::pair<Time, uint32_t>> visit;
  visit.push_back({Time(0, cycle.duration), root});

  while (!visit.empty()) {
    auto item = visit.front();
    visit.pop_front();

    auto& time = std::get<0>(item);
    auto& value = std::get<1>(item);

    if (Pair::predicate(value)) {
      auto head = Pair::head(value);
      auto tail = Pair::tail(value);
      if (head == tail) {
        continue;
      }
      auto start = time.start;
      auto interval = time.duration / (tail - head);
      auto begin = heap.begin();
      for (auto it = begin + head; it != begin + tail; ++it) {
        visit.push_back({Time(start, interval), *it});
        start += interval;
      }
    } else {
      Event event;
      event.cycle = cycle.number;
      event.start = time.start;
      event.duration = time.duration;

      if (Integer::predicate(value)) {
        auto num = Integer::get(value);
        if (Instruction::predicate(value) && num == REST) {
          continue;
        }
        event.value = num;
      } else {
        event.value = Float::get(value);
      }

      events.push_back(event);
    }
  }

  return events;
}
