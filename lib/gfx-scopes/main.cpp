#include <cstring>
#include "hans/engine/object.hpp"

using namespace hans;
using namespace hans::engine;

#define MAX_FRAMES 30
#define ARG_LEFT 0xe7add5f891f566b       /* left */
#define ARG_RIGHT 0xc6ef9eb8bc8afb11     /* right */
#define SHADERS_OSC 0x84f56ca0bb2363c8   /* scopes/shaders/oscilloscope */
#define SHADERS_PHASE 0xe1a6fd5f260d183c /* scopes/shaders/phasescope */
#define SHADERS_FRAG 0x6f9314e2cc69a7e6  /* scopes/shaders/fragment */

struct ScopeState {
  GLuint vao;
  graphics::FBO fbo;
  hash vertex_shader_name;
  hash left;
  hash right;
  Register outlet;
  graphics::ShaderProgram program;
  audio::sample* samples;
  float buffer_length;
  GLuint audio_buffer_object;
  GLuint buffer_length_loc;
};

class OscScopeObject : protected GraphicsObject {
  friend class hans::engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override;
  virtual void draw(Engine& engine) const override;

 private:
  ScopeState state;
};

class PhaseScopeObject : protected GraphicsObject {
  friend class hans::engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override;
  virtual void draw(Engine& engine) const override;

 private:
  ScopeState state;
};

static void parse_args(IPatcher& patcher, ScopeState& state) {
  for (const auto& arg : patcher.arguments()) {
    if (arg.name == ARG_LEFT && arg.type == Argument::Types::STRING) {
      state.left = arg.string;
    } else if (arg.name == ARG_RIGHT && arg.type == Argument::Types::STRING) {
      state.right = arg.string;
    }
  }
}

void OscScopeObject::create(IPatcher& patcher) {
  state.left = 0;
  state.right = 0;
  state.buffer_length = 0;
  state.vertex_shader_name = SHADERS_OSC;
  parse_args(patcher, state);
  patcher.request(IPatcher::Resources::OUTLET, 1);
}

void OscScopeObject::setup(Engine& engine) {
  auto blocksize = engine.config->blocksize;
  auto channels = 2;
  auto max_channel_samples = blocksize * MAX_FRAMES;
  auto max_points = max_channel_samples * channels;

  state.outlet = engine.registers->make(id, Register::Types::OUTLET, 0);
  state.fbo = engine.fbos->make(id);
  state.samples = new audio::sample[max_points];
  state.buffer_length = max_points;

  auto texture = engine.fbos->get_color_attachment(state.fbo, 0);
  engine.registers->write(state.outlet, &texture);

  glGenVertexArrays(1, &state.vao);
  glBindVertexArray(state.vao);

  auto sample_index = new float[max_points];
  for (auto t = 0; t < max_points; ++t) {
    sample_index[t] = t % max_channel_samples;
  }

  auto size = sizeof(float) * max_points;

  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, size, sample_index, GL_STATIC_DRAW);
  delete[] sample_index;

  auto channel_index = new float[max_points];
  for (auto c = 0; c < channels; ++c) {
    auto offset = c * max_channel_samples;
    for (auto t = 0; t < max_channel_samples; ++t) {
      channel_index[offset + t] = t;
    }
  }

  GLuint channel_buffer_object;
  glGenBuffers(1, &channel_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, channel_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, channel_index, GL_STATIC_DRAW);
  delete[] channel_index;

  glGenBuffers(1, &state.audio_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = engine.shaders->create(state.vertex_shader_name);
  auto frag_shdr = engine.shaders->create(SHADERS_FRAG);
  state.program = engine.shaders->create(vert_shdr, frag_shdr);
  glUseProgram(state.program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  pos = glGetAttribLocation(state.program.handle, "index");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, channel_buffer_object);
  pos = glGetAttribLocation(state.program.handle, "channel");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  pos = glGetAttribLocation(state.program.handle, "sample");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  state.buffer_length_loc =
      glGetUniformLocation(state.program.handle, "audio_buffer_length");
}

static uint8_t read_ring_buffer(ScopeState& state, Engine& engine, hash rb_name,
                                size_t offset) {
  auto blocksize = engine.config->blocksize;
  auto framesize = blocksize * sizeof(audio::sample);
  auto available = engine.ring_buffers->available(rb_name);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = engine.ring_buffers->read(rb_name, i);
    auto dest = &state.samples[(i * blocksize) + offset];
    std::memcpy(dest, buffer, framesize);
  }

  return available;
}

void OscScopeObject::update(Engine& engine) {
  auto blocksize = engine.config->blocksize;

  auto read = 0;
  read += read_ring_buffer(state, engine, state.right, 0);
  read += read_ring_buffer(state, engine, state.left, read * blocksize);

  state.buffer_length = read * blocksize;

  auto size = sizeof(audio::sample) * state.buffer_length;
  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  glBufferSubData(GL_ARRAY_BUFFER, 0, size, state.samples);
}

void OscScopeObject::draw(Engine& engine) const {
  glUseProgram(state.program.handle);
  glUniform1f(state.buffer_length_loc, state.buffer_length);

  engine.fbos->bind_fbo(state.fbo);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glPointSize(3);
  glDrawArrays(GL_POINTS, 0, state.buffer_length);
}

void PhaseScopeObject::create(IPatcher& patcher) {
  state.left = 0;
  state.right = 0;
  state.buffer_length = 0;
  state.vertex_shader_name = SHADERS_PHASE;
  parse_args(patcher, state);
  patcher.request(IPatcher::Resources::OUTLET, 1);
}

void PhaseScopeObject::setup(Engine& engine) {
  auto blocksize = engine.config->blocksize;
  auto channels = 2;

  auto max_points = blocksize * MAX_FRAMES * channels;

  state.outlet = engine.registers->make(id, Register::Types::OUTLET, 0);
  state.fbo = engine.fbos->make(id);
  state.samples = new audio::sample[max_points];
  state.buffer_length = max_points / channels;

  auto texture = engine.fbos->get_color_attachment(state.fbo, 0);
  engine.registers->write(state.outlet, &texture);

  glGenVertexArrays(1, &state.vao);
  glBindVertexArray(state.vao);

  auto size = sizeof(float) * max_points;
  glGenBuffers(1, &state.audio_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = engine.shaders->create(state.vertex_shader_name);
  auto frag_shdr = engine.shaders->create(SHADERS_FRAG);
  state.program = engine.shaders->create(vert_shdr, frag_shdr);
  glUseProgram(state.program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  pos = glGetAttribLocation(state.program.handle, "sample");
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  state.buffer_length_loc =
      glGetUniformLocation(state.program.handle, "audio_buffer_length");
}

void PhaseScopeObject::update(Engine& engine) {
  auto blocksize = engine.config->blocksize;
  auto available = engine.ring_buffers->available(state.left);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto left = engine.ring_buffers->read(state.left, i);
    auto right = engine.ring_buffers->read(state.right, i);
    auto block = (i * blocksize);
    auto j = 0;

    for (auto t = 0; t < blocksize; ++t) {
      state.samples[block + j] = left[t];
      state.samples[block + j + 1] = right[t + 1];
      j += 2;
    }
  }

  state.buffer_length = available * blocksize;

  auto size = sizeof(audio::sample) * (available + available) * blocksize;
  glBindBuffer(GL_ARRAY_BUFFER, state.audio_buffer_object);
  glBufferSubData(GL_ARRAY_BUFFER, 0, size, state.samples);
}

void PhaseScopeObject::draw(Engine& engine) const {
  glUseProgram(state.program.handle);
  glUniform1f(state.buffer_length_loc, state.buffer_length);

  engine.fbos->bind_fbo(state.fbo);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glPointSize(3);
  glDrawArrays(GL_POINTS, 0, state.buffer_length);
}

HANS_PLUGIN_INIT(LibraryManager* library) {
  library->add_object<ScopeState, OscScopeObject>("gfx-oscilloscope");
  library->add_object<ScopeState, PhaseScopeObject>("gfx-phasescope");
}
