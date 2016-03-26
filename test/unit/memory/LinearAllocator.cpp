#include "hans/memory/LinearAllocator.hpp"
#include <catch.hpp>

using namespace hans;

SCENARIO("Linear memory allocation", "[memory]") {
  GIVEN("An allocator of a given size") {
    memory::LinearAllocator allocator(32);

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
