#ifndef HANS_ENGINE_OBJECT_H
#define HANS_ENGINE_OBJECT_H

#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/vector.hpp>
#include "hans/common/primitives.hpp"
#include "hans/engine/Engine.hpp"
#include "hans/engine/Patcher.hpp"
#include "hans/engine/gl.hpp"

namespace hans {
namespace engine {

class Object {
 public:
  Object(ObjectDef::ID _id) : id(_id) {
  }
  virtual ~Object() {
  }
  virtual void create(IPatcher& patcher) = 0;
  virtual void setup(Engine& engine) = 0;

 protected:
  const ObjectDef::ID id;
};

class AudioObject : public Object {
 public:
  using Object::Object;
  virtual void update(Engine& engine) = 0;
  virtual void callback(Engine& engine) = 0;
};

class GraphicsObject : public Object {
 public:
  using Object::Object;
  virtual void update(Engine& engine) = 0;
  virtual void draw(Engine& engine) const = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_OBJECT_H
