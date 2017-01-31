#ifndef HANS_ENGINE_OBJECT_H
#define HANS_ENGINE_OBJECT_H

#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/vector.hpp>
#include "hans/engine/configurator.hpp"
#include "hans/engine/context.hpp"
#include "hans/engine/gl.hpp"

namespace hans {
namespace engine {

class Object {
 public:
  Object(ObjectDef::ID _id) : id(_id) {
  }
  virtual ~Object() {
  }
  virtual void create(Configurator& config) = 0;
  virtual void setup(context& ctx) = 0;

 protected:
  const ObjectDef::ID id;
};

class AudioObject : public Object {
 public:
  using Object::Object;
  virtual void update(context& ctx) = 0;
  virtual void callback(context& ctx) = 0;
};

class GraphicsObject : public Object {
 public:
  using Object::Object;
  virtual void update(context& ctx) = 0;
  virtual void draw(context& ctx) const = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_OBJECT_H
