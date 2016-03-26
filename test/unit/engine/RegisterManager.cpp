#include "hans/engine/RegisterManager.hpp"
#include <catch.hpp>

using namespace hans;

SCENARIO("Creating object connections", "[connections]") {
  GIVEN("a set of connected objects") {
    engine::RegisterManager registers(sizeof(uint32_t));
    //      [3]
    //     /   \
    //   [2]   [1]
    //     \   /
    //      [4]
    // source, outlet, sink, inlet
    hans_object_connection connections[4] = {
        {3, 0, 2, 0}, {3, 1, 1, 0}, {2, 0, 4, 0}, {1, 0, 4, 1},
    };

    registers.set_interference_graph((hans_object_connection*)&connections, 4);

    hans_register_handle object_3_outlet_0;
    hans_register_handle object_3_outlet_1;
    hans_register_handle object_2_inlet;
    hans_register_handle object_2_outlet;
    hans_register_handle object_1_inlet;
    hans_register_handle object_1_outlet;
    hans_register_handle object_4_inlet_0;
    hans_register_handle object_4_inlet_1;

    REQUIRE(registers.assign_write_reg(object_3_outlet_0, 3, 0) == true);
    REQUIRE(registers.assign_write_reg(object_3_outlet_1, 3, 1) == true);

    REQUIRE(registers.assign_read_reg(object_2_inlet, 2, 0) == true);
    REQUIRE(registers.assign_write_reg(object_2_outlet, 2, 0) == true);

    REQUIRE(registers.assign_read_reg(object_1_inlet, 1, 0) == true);
    REQUIRE(registers.assign_write_reg(object_1_outlet, 1, 0) == true);

    REQUIRE(registers.assign_read_reg(object_4_inlet_0, 4, 0) == true);
    REQUIRE(registers.assign_read_reg(object_4_inlet_1, 4, 1) == true);

    WHEN("writing and reading data") {
      // Write some data to the root objects registers
      auto f0 = 960;
      bool r0 = registers.set_write_reg(object_3_outlet_0, &f0);

      auto f1 = 256;
      bool r1 = registers.set_write_reg(object_3_outlet_1, &f1);

      // Read data back from registers that should be full
      auto r2 = registers.get_read_reg(object_2_inlet);
      auto f2 = *static_cast<const uint32_t*>(r2);

      auto r3 = registers.get_read_reg(object_1_inlet);
      auto f3 = *static_cast<const uint32_t*>(r3);

      // Read data back from registers that should be blank
      auto r4 = registers.get_read_reg(object_2_outlet);
      auto f4 = *static_cast<const uint32_t*>(r4);

      auto r5 = registers.get_read_reg(object_1_outlet);
      auto f5 = *static_cast<const uint32_t*>(r5);

      auto r6 = registers.get_read_reg(object_4_inlet_0);
      auto f6 = *static_cast<const uint32_t*>(r6);

      auto r7 = registers.get_read_reg(object_4_inlet_1);
      auto f7 = *static_cast<const uint32_t*>(r7);

      THEN("then its value should change") {
        REQUIRE(r0 == true);
        REQUIRE(r1 == true);

        REQUIRE(f2 == 960);
        REQUIRE(f3 == 256);
        REQUIRE(f4 == 0);
        REQUIRE(f5 == 0);
        REQUIRE(f6 == 0);
        REQUIRE(f7 == 0);
      }
    }
  }
}
