#ifndef VISUALISERS_H_
#define VISUALISERS_H_

#include <unordered_map>
#include "buffer.hpp"
#include "hans/gl.hpp"
#include "hans/object.hpp"

namespace visualisers {

class PointsVisualiser {
 public:
  static constexpr uint64_t POSITION = 0x8bbeb160190f613a;
  static constexpr uint64_t COLOR = 0x6776ddaf0290228;

  struct Options {
    uint64_t position = 0;
    uint64_t color = 0;
  };

  void setup(hans::context& ctx);
  void set(vm::Buffer& buffer, Options& options);
  void draw() const;

 private:
  void initialize(vm::Buffer& buffer, Options& options);

  GLuint _vao;
  std::unordered_map<uint64_t, GLuint> _buffers;
  hans::graphics::ShaderProgram _shader;
  uint64_t _num_particles = 0;
};

} // namespace visualisers

#endif // VISUALISERS_H_
