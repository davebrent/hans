#include "hans/interpreter.hpp"
#include <algorithm>
#include <istream>
#include <iterator>
#include <random>
#include <stdexcept>

using namespace hans;
using namespace hans::interpreter;

List::List() : start(0), end(0){};
List::List(size_t start, size_t end) : start(start), end(end){};

Value::Value(uint32_t number) : type(NUMBER), number(number){};
Value::Value(List list) : type(LIST), list(list){};

Value DStack::pop() {
  auto v = buffer.back();
  buffer.pop_back();
  pointer--;
  return v;
}

void DStack::push(Value value) {
  buffer.push_back(value);
  pointer++;
}

bool DStack::empty() {
  return buffer.empty();
}

Interpreter::Interpreter(Cycle& cycle, IStack istack)
    : cycle(cycle), istack(istack){};

static void number(Interpreter& itp, uint32_t code) {
  if (code > REST) {
    throw std::runtime_error(
        "Unrecognised instruction, remember, values must be unsinged 16bit "
        "integers");
  }
  Value value(code);
  itp.dstack.push(value);
}

static void begin(Interpreter& itp, uint32_t code) {
  Value value(BEGIN);
  itp.dstack.push(value);
}

static void end(Interpreter& itp, uint32_t code) {
  List root;
  root.start = itp.heap.size();

  while (true) {
    auto value = itp.dstack.pop();

    if (value.type == Value::NUMBER && value.number == BEGIN) {
      auto begin = itp.heap.begin();
      root.end = itp.heap.size();
      std::reverse(begin + root.start, begin + root.end);
      itp.dstack.push(Value(root));
      return;
    }

    itp.heap.push_back(value);
  }
}

static void rest(Interpreter& itp, uint32_t code) {
  Value value(REST);
  itp.dstack.push(value);
}

static void add(Interpreter& itp, uint32_t code) {
  auto rhs = itp.dstack.pop();
  auto lhs = itp.dstack.pop();
  Value value(lhs.number + rhs.number);
  itp.dstack.push(value);
}

static void ref(Interpreter& itp, uint32_t code) {
  auto next = itp.istack.front();
  itp.istack.pop_front();
  Value value(next);
  itp.dstack.push(value);
}

static void duration(Interpreter& itp, uint32_t code) {
  itp.cycle.duration = itp.dstack.pop().number;
}

static void call(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.pop();
  itp.istack.push_back(value.number);
}

static void repeat(Interpreter& itp, uint32_t code) {
  auto n = itp.dstack.pop().number;
  auto value = itp.dstack.pop();

  List root;
  root.start = itp.heap.size();
  root.end = root.start + n;

  for (auto i = 0; i < n; ++i) {
    itp.heap.push_back(value);
  }

  itp.dstack.push(Value(root));
}

static void every(Interpreter& itp, uint32_t code) {
  // XXX: else clause is letf on the dstack
  auto freq = itp.dstack.pop().number;
  if (freq % itp.cycle.number == 0) {
    itp.dstack.pop();
    auto value = itp.dstack.pop();
    itp.istack.push_back(value.number);
  }
}

static void reverse(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.pop();
  auto begin = itp.heap.begin();
  std::reverse(begin + value.list.start, begin + value.list.end);
  itp.dstack.push(value);
}

static void shuffle(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.pop();
  auto begin = itp.heap.begin();
  std::random_device rd;
  std::mt19937 g(rd());
  std::shuffle(begin + value.list.start, begin + value.list.end, g);
  itp.dstack.push(value);
}

static void rotate(Interpreter& itp, uint32_t code) {
  auto amount = itp.dstack.pop().number;
  auto value = itp.dstack.pop();

  if (value.list.start != value.list.end) {
    amount %= value.list.end - value.list.start;
    auto begin = itp.heap.begin();
    std::rotate(begin + value.list.start, begin + value.list.start + amount,
                begin + value.list.end);
  }

  itp.dstack.push(value);
}

static void degrade(Interpreter& itp, uint32_t code) {
  auto amount = itp.dstack.pop().number;
  auto value = itp.dstack.pop();

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 100);

  auto begin = itp.heap.begin();
  for (auto it = begin + value.list.start; it != begin + value.list.end; ++it) {
    if (it->type != Value::NUMBER && it->number != REST && dis(gen) < amount) {
      it->type = Value::NUMBER;
      it->number = REST;
    }
  }

  itp.dstack.push(value);
}

static void cycle(Interpreter& itp, uint32_t code) {
  auto list = itp.dstack.pop().list;
  if (list.end != list.start) {
    auto i = itp.cycle.number % (list.end - list.start);
    itp.dstack.push(itp.heap.at(i));
  }
}

static void palindrome(Interpreter& itp, uint32_t code) {
  if (itp.cycle.number % 2 == 1) {
    reverse(itp, code);
  }
}

IStack hans::interpreter::compile(std::istream& is) {
  // XXX: Be more forgiving with input, dont use exceptions for control flow
  //      rethink this function some...
  IStack istack;

  std::vector<std::string> tokens;
  std::copy(std::istream_iterator<std::string>(is),
            std::istream_iterator<std::string>(), std::back_inserter(tokens));

  for (auto str : tokens) {
    try {
      auto num = std::stoul(str);
      istack.push_back(num);
    } catch (const std::exception& e) {
      auto it = std::find(Words.begin(), Words.end(), str);
      if (it == Words.end()) {
        throw std::runtime_error("Bad token");
      }
      istack.push_back((it - Words.begin()) + REST);
    }
  }

  return istack;
}

void hans::interpret(Interpreter& itp) {
  while (!itp.istack.empty()) {
    auto code = itp.istack.front();
    itp.istack.pop_front();

    switch (code) {
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
    case REF:
      ref(itp, code);
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
    default:
      number(itp, code);
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
                                       const std::vector<Value>& heap,
                                       const Value& root) {
  EventList events;
  std::deque<std::pair<Time, Value>> visit;
  visit.push_back({Time(0, cycle.duration), root});

  while (!visit.empty()) {
    auto item = visit.front();
    visit.pop_front();

    auto& time = std::get<0>(item);
    auto& value = std::get<1>(item);

    switch (value.type) {
    case Value::LIST: {
      if (value.list.start == value.list.end) {
        break;
      }
      auto start = time.start;
      auto interval = time.duration / (value.list.end - value.list.start);
      auto begin = heap.begin();
      for (auto it = begin + value.list.start; it != begin + value.list.end;
           ++it) {
        visit.push_back({Time(start, interval), *it});
        start += interval;
      }
      break;
    }

    case Value::NUMBER:
      if (value.number == REST) {
        break;
      }
      Event event;
      event.cycle = cycle.number;
      event.start = time.start;
      event.duration = time.duration;
      event.value = value.number;
      events.push_back(event);
      break;
    }
  }

  return events;
}
