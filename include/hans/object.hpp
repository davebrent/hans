#ifndef HANS_OBJECT_H
#define HANS_OBJECT_H

#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/vector.hpp>
#include "hans/configurator.hpp"
#include "hans/context.hpp"
#include "hans/gl.hpp"

namespace hans {

class Object {
 public:
  Object(ObjectDef::ID _id) : id(_id) {
  }
  virtual ~Object() {
  }
  virtual void create(IConfigurator& config) = 0;
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

} // namespace hans

#endif // HANS_OBJECT_H
