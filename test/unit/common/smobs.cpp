#include "hans/common/smobs.hpp"
#include <libguile.h>
#include <catch.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <string>

using namespace hans;

struct TestObject {
  size_t a = 1;
  float b = 2.5;
  float c;

  TestObject(float d, int scaler) {
    c = d * scaler;
  }

  template <class Archive>
  void serialize(Archive& ar) {
    ar(CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(c));
  }
};

struct SimpleObject {
  int a = 1;
  int b = 3;
  int c = 5;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(c));
  }
};

TEST_CASE("Smobs", "[scm]") {
  SECTION("Custom constructors") {
    bool constructed = false;
    scm::smob<TestObject>("test-object", [&](void* bytes, SCM args) {
      constructed = true;
      auto d = scm_to_double(scm_list_ref(args, scm_from_int(0)));
      auto s = scm_to_int(scm_list_ref(args, scm_from_int(1)));

      REQUIRE(d == 3.14);
      REQUIRE(s == 3);
      new (bytes) TestObject(d, s);
    });

    SCM result = scm_c_eval_string(R"(
      (let ((object? #f))
        (let ((obj (make-hans-object 'test-object '(3.14 3))))
          (set! object? (hans-object? obj 'test-object)))
        (gc)
        object?)
    )");

    REQUIRE(constructed == true);
    REQUIRE(scm_to_bool(result) == 1);
  }

  SECTION("Object types") {
    scm::smob<SimpleObject>("simple-object");

    SCM result = scm_c_eval_string(R"(
      (let ((obj (make-hans-object 'simple-object '())))
        (eq? (hans-object-type obj) 'simple-object))
    )");

    REQUIRE(scm_to_bool(result) == 1);
  }

  SECTION("Getting fields") {
    scm::smob<SimpleObject>("simple-object");

    SCM result = scm_c_eval_string(R"(
      (let ((obj (make-hans-object 'simple-object '())))
        (%hans-object-get obj))
    )");

    auto str = scm_to_locale_string(result);
    auto xml = std::string(str);
    auto expected = std::string(R"(
<?xml version="1.0" encoding="utf-8"?>
<cereal>
	<value0 type="SimpleObject">
		<a type="int">1</a>
		<b type="int">3</b>
		<c type="int">5</c>
	</value0>
</cereal>)");

    REQUIRE(xml.find(expected) != -1);
    std::free(str);
  }

  SECTION("Setting fields") {
    scm::smob<SimpleObject>("simple-object");

    SCM result = scm_c_eval_string(R"(
    (let ((obj (make-hans-object 'simple-object '())))
      (%set-hans-object! obj "<?xml version='1.0' encoding='utf-8'?>
<cereal>
	<value0>
		<a>2</a>
		<b>6</b>
		<c>10</c>
	</value0>
</cereal>")
      obj)
    )");

    auto obj = scm::to_cpp<SimpleObject>(result);
    REQUIRE(obj.a == 2);
    REQUIRE(obj.b == 6);
    REQUIRE(obj.c == 10);
  }
}
