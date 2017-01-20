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
    def.type = ObjectDef::GRAPHICS;
    def.size = sizeof(TestState);

    auto objects = ListView<ObjectDef>(&def, 1);
    PluginManager plugins(strings, objects);
    plugins.add_object<TestState, TestObject>("test-object");

    auto inst_1 = def.create(0, "");
    static_cast<TestObject*>(inst_1)->state.foo = 123456;
    REQUIRE(def.serialize(inst_1) == "AUDiAQA=");
    def.destroy(inst_1);

    auto inst_2 = def.create(0, "AUDiAQA=");
    REQUIRE(static_cast<TestObject*>(inst_2)->state.foo == 123456);
    def.destroy(inst_2);
  }
}
