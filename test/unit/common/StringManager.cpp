#include "hans/common/StringManager.hpp"
#include <catch.hpp>

using namespace hans;

SCENARIO("Interning strings", "[strings]") {
  GIVEN("strings and a manager") {
    common::StringManager string_manager(64);

    WHEN("interning") {
      const char* str1 = "hello";
      const unsigned char* str2 = reinterpret_cast<const unsigned char*>(str1);
      std::string str3 = "foobar";

      THEN("the strings hash should be returned") {
        REQUIRE(string_manager.intern(str1) == 0x1E68D17C457BF117);
        REQUIRE(string_manager.intern(str2) == 0x1E68D17C457BF117);
        REQUIRE(string_manager.intern(str3.c_str()) == 0xD49F461720D7A196);
      }
    }

    WHEN("hashing large strings") {
      const char* str1 = "hellohellohellohellohellohellohellohellohellohello";
      THEN("the strings hash should be returned") {
        REQUIRE(string_manager.intern(str1) == 0xC96A478EEE0CEF86);
      }
    }

    WHEN("looking up") {
      const char* str1 = "hello";
      hans_hash symbol = string_manager.intern(str1);
      const char* str2 = string_manager.lookup(symbol);

      THEN("the strings hash should be returned") {
        REQUIRE(str2[0] == 'h');
        REQUIRE(str2[1] == 'e');
        REQUIRE(str2[2] == 'l');
        REQUIRE(str2[3] == 'l');
        REQUIRE(str2[4] == 'o');
      }
    }

    WHEN("Null terminating strings") {
      const char* str1 = "hello";
      const char* str2 = "world";
      hans_hash symbol1 = string_manager.intern(str1);
      hans_hash symbol2 = string_manager.intern(str2);
      const char* str3 = string_manager.lookup(symbol1);

      THEN("the strings hash should be returned") {
        REQUIRE(str3[5] == '\0');
      }
    }
  }
}
