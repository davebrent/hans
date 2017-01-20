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

static TestObject* to_test(Object* object) {
  return reinterpret_cast<TestObject*>(object);
}

TEST_CASE("engine objects", "[object]") {
  SECTION("serializing state") {
    StringManager strings(12);

    ObjectDef def;
    def.name = strings.intern("test-object");
    def.type = ObjectDef::GRAPHICS;
    def.size = sizeof(TestState);
    std::vector<ObjectDef> objects;
    objects.push_back(std::move(def));

    PluginManager plugins(strings, objects);
    plugins.add_object<TestState, TestObject>("test-object");

    auto inst_1 = plugins.create(def.name);

    to_test(inst_1)->state.foo = 123456;
    REQUIRE(plugins.serialize(def.name, inst_1) == "AUDiAQA=");
    plugins.destroy(def.name, inst_1);

    auto inst_2 = plugins.create(def.name, 0, "AUDiAQA=");
    REQUIRE(to_test(inst_2)->state.foo == 123456);
    plugins.destroy(def.name, inst_2);
  }
}