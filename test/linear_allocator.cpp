#include "hans/linear_allocator.hpp"
#include <catch.hpp>

using namespace hans;

SCENARIO("Linear memory allocation", "[common]") {
  GIVEN("An allocator of a given size") {
    LinearAllocator allocator(32);

    WHEN("allocating memory") {
      void* a = allocator.allocate(16);
      void* b = allocator.allocate(16);
      void* c = allocator.allocate(1);

      THEN("memory should not be null") {
        REQUIRE(a != nullptr);
        REQUIRE(b != nullptr);
        REQUIRE(c == nullptr);
      }
    }
  }
}
