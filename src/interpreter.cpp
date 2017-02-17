#include "hans/interpreter.hpp"
#include <algorithm>
#include <istream>
#include <iterator>
#include <random>

using namespace hans;
using namespace hans::interpreter;

Value::Value() : type(UNDEFINED), number(0) {};
Value::Value(size_t number) : type(NUMBER), number(number){};
Value::Value(Tree tree) : type(TREE), tree(tree), number(0) {};

Value DStack::pop() {
  auto v = buffer.back();
  buffer.pop_back();
  pointer--;
  return v;
}

Value DStack::peek_back() {
  if (pointer > 0) {
    return buffer.at(pointer - 1);
  }
  return Value();
}

void DStack::push(Value value) {
  buffer.push_back(value);
  pointer++;
}

bool DStack::empty() {
  return buffer.empty();
}

Interpreter::Interpreter(sequencer::Cycle& cycle, IStack istack)
    : cycle(cycle), istack(istack){};

static void number(Interpreter& itp, uint32_t code) {
  if (code > REST_VALUE) {
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
  Tree tree;
  while (true) {
    auto previous = itp.dstack.peek_back();
    if (previous.type == Value::UNDEFINED) {
      break;
    }

    if (previous.type == Value::NUMBER && previous.number == BEGIN) {
      std::reverse(tree.children.begin(), tree.children.end());
      Value value(tree);
      itp.dstack.pop();
      itp.dstack.push(value);
      break;
    }

    if (previous.type == Value::TREE) {
      tree.children.push_back(itp.dstack.pop().tree);
    } else if (previous.type == Value::NUMBER) {
      Tree n;
      n.value = itp.dstack.pop().number;
      tree.children.push_back(n);
    } else {
      throw std::runtime_error("END stack error, unknown type");
    }
  }
}

static void rest(Interpreter& itp, uint32_t code) {
  Value value(REST_VALUE);
  itp.dstack.push(value);
}

static void add(Interpreter& itp, uint32_t code) {
  auto rhs = itp.dstack.pop();
  auto lhs = itp.dstack.pop();

  if (rhs.type == Value::TREE) {
    Tree tree;
    tree.children.push_back(lhs.tree);
    tree.children.push_back(rhs.tree);
    Value value(tree);
    itp.dstack.push(value);
  } else if (rhs.type == Value::NUMBER) {
    Value value(lhs.number + rhs.number);
    itp.dstack.push(value);
  }
}

static void ref(Interpreter& itp, uint32_t code) {
  auto next = itp.istack.front();
  itp.istack.pop_front();
  Value value(next);
  itp.dstack.push(value);
}

static void duration(Interpreter& itp, uint32_t code) {
  auto dur = itp.dstack.pop().number;
  itp.cycle.duration.store(dur);
}

static void call(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.pop();
  itp.istack.push_back(value.number);
}

static void repeat(Interpreter& itp, uint32_t code) {
  auto n = itp.dstack.pop().number;
  auto t = itp.dstack.pop().tree;

  Tree tree;
  for (auto i = 0; i < n; ++i) {
    tree.children.push_back(t);
  }

  Value value(tree);
  itp.dstack.push(value);
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
  std::reverse(value.tree.children.begin(), value.tree.children.end());
  itp.dstack.push(value);
}

static void shuffle(Interpreter& itp, uint32_t code) {
  auto value = itp.dstack.pop();
  std::random_device rd;
  std::mt19937 g(rd());
  std::shuffle(value.tree.children.begin(), value.tree.children.end(), g);
  itp.dstack.push(value);
}

static void rotate(Interpreter& itp, uint32_t code) {
  auto amount = itp.dstack.pop().number;
  auto value = itp.dstack.pop();
  amount %= value.tree.children.size();
  std::rotate(value.tree.children.begin(), value.tree.children.begin() + amount,
              value.tree.children.end());
  itp.dstack.push(value);
}

static void degrade(Interpreter& itp, uint32_t code) {
  auto amount = itp.dstack.pop().number;
  auto value = itp.dstack.pop();

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 100);

  for (auto& node : value.tree.children) {
    if (dis(gen) < amount) {
      node.value = REST_VALUE;
      node.children.clear();
    }
  }

  itp.dstack.push(value);
}

static void cycle(Interpreter& itp, uint32_t code) {
  auto tree = itp.dstack.pop().tree;
  auto index = itp.cycle.number % tree.children.size();
  Value value(tree.children.at(index));
  itp.dstack.push(value);
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
      istack.push_back((it - Words.begin()) + 0x10000);
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
    default: {
      number(itp, code);
      break;
    }
    };
  }
}

struct Time {
  float start;
  float duration;
  Time(float start, float duration) : start(start), duration(duration){};
};

sequencer::EventList hans::interpreter::to_events(const sequencer::Cycle& cycle,
                                                  const Tree& tree) {
  sequencer::EventList events;
  std::deque<std::pair<Time, Tree>> visit;
  Time start(0, cycle.duration);
  visit.push_back({start, tree});

  while (!visit.empty()) {
    auto item = visit.front();
    visit.pop_front();

    auto& time = std::get<0>(item);
    auto& tree = std::get<1>(item);

    if (tree.children.size() == 0 && tree.value != 0xffff) {
      sequencer::Event event;
      event.cycle = cycle.number;
      event.start = time.start;
      event.duration = time.duration;
      event.value = tree.value;
      events.push_back(event);
    } else {
      auto interval = time.duration / tree.children.size();
      auto start = time.start;

      for (const auto& child : tree.children) {
        visit.push_back({Time(start, interval), child});
        start += interval;
      }
    }
  }

  return events;
}
