#ifndef HANS_GRAPHICSDEBUG_H_
#define HANS_GRAPHICSDEBUG_H_

#include "hans/strings.hpp"

namespace hans {

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

} // namespace hans

#endif // HANS_GRAPHICSDEBUG_H_
