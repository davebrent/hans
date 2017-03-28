#include "visualisers.hpp"
#define EIGEN_NO_MALLOC 1
#include <Eigen/Dense>

#define POINTS_SHADER_VERTEX 0x726706005127b9b8 /* particles/points/vertex */
#define POINTS_SHADER_FRAGMENT \
  0xf1c34face9f1be13 /* particles/points/fragment */
#define POINTS_SHADER_GEOMETRY \
  0xfb8d540767e04a24 /* particles/points/geometry */

using namespace visualisers;

template <typename T>
static GLenum gl_value() {
  switch (sizeof(T)) {
  case 4:
    return GL_FLOAT;
  case 8:
    return GL_DOUBLE;
  default:
    throw std::runtime_error("Unsupported GL type");
  }
}

static uint64_t columns(vm::Buffer& buffer) {
  auto cols = 0;
  for (auto i = 0; i < buffer.channels; ++i) {
    cols += buffer.components[i];
  }
  return cols;
}

void PointsVisualiser::setup(hans::context& ctx) {
  auto vert_shader = ctx.shaders.create(POINTS_SHADER_VERTEX);
  auto frag_shader = ctx.shaders.create(POINTS_SHADER_FRAGMENT);
  _shader = ctx.shaders.create(vert_shader, frag_shader);
  glUseProgram(_shader.handle);
  glGenVertexArrays(1, &_vao);
  // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=61143 ?
  _buffers.reserve(1);
}

void PointsVisualiser::initialize(vm::Buffer& buffer, Options& options) {
  auto bytes = sizeof(vm::Buffer::Value) * buffer.size * columns(buffer);

  glBindVertexArray(_vao);
  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, bytes, buffer.data, GL_DYNAMIC_DRAW);
  _buffers[buffer.id] = vbo;

  auto position_x = glGetAttribLocation(_shader.handle, "position_x");
  auto position_y = glGetAttribLocation(_shader.handle, "position_y");
  auto color_r = glGetAttribLocation(_shader.handle, "color_r");
  auto color_g = glGetAttribLocation(_shader.handle, "color_g");
  auto color_b = glGetAttribLocation(_shader.handle, "color_b");
  auto color_a = glGetAttribLocation(_shader.handle, "color_a");

  auto type = gl_value<vm::Buffer::Value>();
  auto b = sizeof(vm::Buffer::Value);
  auto s = buffer.size;

  char* ptr = 0;
  for (auto i = 0; i < buffer.channels; ++i) {
    int components = buffer.components[i];

    if (options.position == i && components == 2) {
      glVertexAttribPointer(position_x, 1, type, GL_FALSE, 0, ptr);
      glVertexAttribPointer(position_y, 1, type, GL_FALSE, 0, ptr + (s * b));
      glEnableVertexAttribArray(position_x);
      glEnableVertexAttribArray(position_y);
    } else if (options.color == i && components == 4) {
      glVertexAttribPointer(color_r, 1, type, GL_FALSE, 0, ptr);
      glVertexAttribPointer(color_g, 1, type, GL_FALSE, 0, ptr + ((s * 1) * b));
      glVertexAttribPointer(color_b, 1, type, GL_FALSE, 0, ptr + ((s * 2) * b));
      glVertexAttribPointer(color_a, 1, type, GL_FALSE, 0, ptr + ((s * 3) * b));
      glEnableVertexAttribArray(color_r);
      glEnableVertexAttribArray(color_g);
      glEnableVertexAttribArray(color_b);
      glEnableVertexAttribArray(color_a);
    }

    ptr += sizeof(vm::Buffer::Value) * buffer.size * components;
  }
}

void PointsVisualiser::set(vm::Buffer& buffer, Options& options) {
  _num_particles = buffer.size;
  auto it = _buffers.find(buffer.id);
  if (it == _buffers.end()) {
    initialize(buffer, options);
  } else {
    auto bytes = sizeof(vm::Buffer::Value) * buffer.size * columns(buffer);
    glBindBuffer(GL_ARRAY_BUFFER, it->second);
    glBufferSubData(GL_ARRAY_BUFFER, 0, bytes, buffer.data);
  }
}

void PointsVisualiser::draw() const {
  glUseProgram(_shader.handle);
  glBindVertexArray(_vao);
  glDrawArrays(GL_POINTS, 0, _num_particles);
}
