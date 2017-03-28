#include "virtual_machine.hpp"
#include <catch.hpp>
#include <chrono>
#include <iostream>
#include <sstream>
#include <vector>
#include "bytecode.hpp"
#include "hans/hasher.hpp"

using namespace vm;

static std::chrono::high_resolution_clock::time_point time_start() {
  return std::chrono::high_resolution_clock::now();
}

static void time_end(std::chrono::high_resolution_clock::time_point start) {
  auto end = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(end - start)
          .count();
  std::cout << duration << " microseconds (" << (duration * 0.001)
            << " milliseconds)" << std::endl;
}

bytecode::Program exp(const std::string& str) {
  std::vector<std::string> tokens;
  std::istringstream ss(str);
  bytecode::Program program;
  bytecode::tokenize(tokens, ss);
  bytecode::compile(program, tokens);
  return program;
}

TEST_CASE("Virtual machine", "[vm]") {
  SECTION("Variables") {
    auto program = exp(R"(
    main:
        3
        =foo
        1
        2
        @foo
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::INTEGER);
    auto result = value.integer;
    REQUIRE(result == 3);
  }

  SECTION("Buffer allocation") {
    auto program = exp(R"(
    main:
        3       ;; Channel 1 components
        2       ;; Channel 2 components
        2       ;; Number of channels
        100     ;; Size of each channel
        buffer
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    REQUIRE(buffer.channels == 2);
    REQUIRE(buffer.components[0] == 3);
    REQUIRE(buffer.components[1] == 2);
    REQUIRE(buffer.size == 100);
  }

  SECTION("Filling channels with constant values") {
    auto program = exp(R"(
    main:
        3 2 2 2 buffer
        [ 100 200 300 ] 0 buffer-fill
        [ 99 98 ]       1 buffer-fill
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{100, 100, 200, 200, 300,
                                        300, 99,  99,  98,  98};
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 10);
    REQUIRE(expected == actual);
  }

  SECTION("Adding buffers") {
    auto program = exp(R"(
    main:
        2 2 2 3 4 buffer
        [ 100 200 ] 0 buffer-fill
        [ 101 202 ] 1 buffer-fill
        0 1 2 buffer-add
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{100, 100, 100, 100, 200, 200, 200, 200,
                                        101, 101, 101, 101, 202, 202, 202, 202,
                                        201, 201, 201, 201, 402, 402, 402, 402};
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 24);
    REQUIRE(expected == actual);
  }

  SECTION("Subtracting buffers") {
    auto program = exp(R"(
    main:
        2 2 2 3 4 buffer
        [ 100 200 ] 0 buffer-fill
        [ 101 202 ] 1 buffer-fill
        0 1 2 buffer-sub
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{100, 100, 100, 100, 200, 200, 200, 200,
                                        101, 101, 101, 101, 202, 202, 202, 202,
                                        -1,  -1,  -1,  -1,  -2,  -2,  -2,  -2};
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 24);
    REQUIRE(expected == actual);
  }

  SECTION("Multiplying buffers") {
    auto program = exp(R"(
    main:
        2 2 2 3 4 buffer
        [ 10 100 ] 0 buffer-fill
        [ 0.1 0.5 ] 1 buffer-fill
        0 1 2 buffer-mul
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{10,  10,  10,  10,  100, 100, 100, 100,
                                        0.1, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5,
                                        1,   1,   1,   1,   50,  50,  50,  50};
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 24);
    REQUIRE(expected == actual);
  }

  SECTION("Dividing buffers") {
    auto program = exp(R"(
    main:
        2 2 2 3 4 buffer
        [ 10 100 ] 0 buffer-fill
        [ 5 2 ] 1 buffer-fill
        0 1 2 buffer-div
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{10, 10, 10, 10, 100, 100, 100, 100,
                                        5,  5,  5,  5,  2,   2,   2,   2,
                                        2,  2,  2,  2,  50,  50,  50,  50};
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 24);
    REQUIRE(expected == actual);
  }

  SECTION("Random buffers") {
    auto program = exp(R"(
    main:
        2 1 4 buffer
        0 random-noise
    )");
    State state;
    state.seed = 21188;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{
        0.658269,  0.455286,  0.965025, 0.854534,
        -0.644038, -0.386038, 0.564507, 0.894798,
    };
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 8);
    for (auto i = 0; i < actual.size(); ++i) {
      REQUIRE(expected.at(i) == Approx(actual.at(i)));
    }
  }

  SECTION("Linear distribution") {
    auto program = exp(R"(
    main:
        2 1 4 buffer
        0   ;; channel
        1   ;; component
        0.0 ;; from
        3.0 ;; to
        distribute-linear
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    auto buffer = value.buffer;
    std::vector<Buffer::Value> expected{
        0.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 2.0, 3.0,
    };
    std::vector<Buffer::Value> actual(buffer.data, buffer.data + 8);
    for (auto i = 0; i < actual.size(); ++i) {
      REQUIRE(expected.at(i) == Approx(actual.at(i)));
    }
  }

  /*
  SECTION("Time it") {
    struct v2 {
      float data[4];
      v2() {
        data[0] = 0;
        data[1] = 0;
        data[2] = 0;
        data[3] = 0;
      }
    };

    auto t0 = time_start();
    auto p0 = exp(R"(
    main:
        4 4 4 3 10000 buffer
        0 random-noise
    )");
    State s0;
    eval(s0, p0, hans::hasher("main"));
    auto v0 = s0.dstack.back();
    REQUIRE(v0.type == Value::BUFFER);
    time_end(t0);

    auto t1 = time_start();
    auto program = exp(R"(
    main:
        4 4 4 3 500000 buffer
        [ 100 200 300 400 ] 0 buffer-fill
        [ 101 202 301 404 ] 1 buffer-fill
        0 1 2 buffer-add
        0 1 2 buffer-mul
    )");
    State state;
    eval(state, program, hans::hasher("main"));
    auto value = state.dstack.back();
    REQUIRE(value.type == Value::BUFFER);
    time_end(t1);

    auto t2 = time_start();
    auto p2 = exp(R"(
    main:
        500000
    )");
    State s2;
    eval(s2, p2, hans::hasher("main"));
    auto size = s2.dstack.back().integer;
    auto channel1 = new v2[size]();
    auto channel2 = new v2[size]();
    auto channel3 = new v2[size]();
    for (auto i = 0; i < size; ++i) {
      channel1[i].data[0] = 100;
      channel1[i].data[1] = 200;
      channel1[i].data[2] = 100;
      channel1[i].data[3] = 200;
    }
    for (auto i = 0; i < size; ++i) {
      channel2[i].data[0] = 101;
      channel2[i].data[1] = 201;
      channel2[i].data[2] = 202;
      channel2[i].data[3] = 203;
    }
    for (auto i = 0; i < size; ++i) {
      channel3[i].data[0] = channel1[i].data[0] + channel1[i].data[0];
      channel3[i].data[1] = channel1[i].data[1] + channel1[i].data[1];
      channel3[i].data[2] = channel1[i].data[2] + channel1[i].data[2];
      channel3[i].data[3] = channel1[i].data[3] + channel1[i].data[3];
    }
    for (auto i = 0; i < size; ++i) {
      channel3[i].data[0] = channel1[i].data[0] * channel1[i].data[0];
      channel3[i].data[1] = channel1[i].data[1] * channel1[i].data[1];
      channel3[i].data[2] = channel1[i].data[2] * channel1[i].data[2];
      channel3[i].data[3] = channel1[i].data[3] * channel1[i].data[3];
    }
    time_end(t2);
  }

  SECTION("Visualiser test") {
    auto program = exp(R"(
    setup:
      3                     ; Position channel
      4                     ; RGBA color channel
      2                     ; 2 channels
      100000                ; with 100,000 elements
      buffer                ; create the buffer
      =buff1                ; Store buffer in global variable

    update:
                            ; Make some changes to the buffer
      @buff1 0 random-noise ; Fill channel 0 with random noise
      @buff1 1 random-noise ; Fill channel 0 with random noise
                            ; Setup drawing the result
      @buff1
      [                     ; Set visualiser options, list of pairs
        $position   0       ; Visualiser has a position attribute, bind to channel 1
        $color      1       ; Set the color attribute to channel 2
      ]
      $points               ; Use the points visualiser
      render                ; Kick off the drawing
    )");
    State state;
    eval(state, program, hans::hasher("setup"));
    eval(state, program, hans::hasher("update"));
  }
  */
}
