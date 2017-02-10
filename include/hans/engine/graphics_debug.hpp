#ifndef HANS_ENGINE_GRAPHICSDEBUG_H_
#define HANS_ENGINE_GRAPHICSDEBUG_H_

#include "hans/engine/strings.hpp"

namespace hans {
namespace engine {

class GraphicsDebug {
 public:
  GraphicsDebug(const StringManager& strings, bool enabled);
  void push(const hash name) const;
  void push(const char* name) const;
  void pop() const;

 private:
  const StringManager& _strings;
  bool _enabled;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_GRAPHICSDEBUG_H_
