#ifndef BUFFER_H_
#define BUFFER_H_

#include <Eigen/Dense>

namespace vm {

constexpr uint8_t MAX_CHANNELS = 24;

struct Buffer {
  using Value = float;
  using Matrix = Eigen::MatrixXf;
  uint64_t id;
  uint64_t size;
  uint8_t channels;
  uint8_t components[MAX_CHANNELS];
  Value* data;
};

} // namespace vm

#endif // BUFFER_H_
