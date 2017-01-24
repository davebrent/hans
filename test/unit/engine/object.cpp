#include "hans/engine/object.hpp"
#include <catch.hpp>
#include "hans/common/StringManager.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

struct TestState {
  int foo;
  Register inlet;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(foo);
  }
};

class TestObject : protected GraphicsObject {
 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override {
  }
  virtual void setup(Engine& engine) override {
  }
  virtual void update(Engine& engine) override {
  }
  virtual void draw(Engine& engine) const override {
  }
  TestState state;
};

TEST_CASE("engine objects", "[object]") {
  SECTION("serializing state") {
    StringManager strings(12);

    ObjectDef def;
    def.name = strings.intern("test-object");

    PluginManager plugins(strings);
    plugins.add_object<TestState, TestObject>("test-object");

    auto duplicate = plugins.add_object<TestState, TestObject>("test-object");
    REQUIRE(duplicate == false);

    auto inst_1 = plugins.construct(def.name);

    reinterpret_cast<TestObject*>(inst_1)->state.foo = 123456;
    REQUIRE(plugins.serialize(def.name, inst_1) == "AUDiAQA=");
    plugins.destruct(def.name, inst_1);

    auto inst_2 = plugins.construct(def.name, 0, "AUDiAQA=");
    REQUIRE(reinterpret_cast<TestObject*>(inst_2)->state.foo == 123456);
    plugins.destruct(def.name, inst_2);
  }
}
