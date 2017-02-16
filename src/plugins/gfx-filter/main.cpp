#include "hans/object.hpp"

#define FILTER 0x357ddc26b51af2ba           /* filter */
#define AMOUNT 0x11ed734ddeee006c           /* amount */
#define SHADER_VERT 0xac2fc0d7c2793744      /* filter/shader/vert */
#define SHADER_FRAG 0x28c5dced1d35512b      /* filter/shader/frag */
#define PASSTHROUGH 0x56b3ebbf4277fe64      /* passthrough */
#define PIXELATE 0x5b44d05452629d5a         /* pixelate */
#define GREYSCALE 0x52938ee09e9f277a        /* greyscale */
#define INVERT 0xb7c4e0bff104a4bf           /* invert */
#define RGB2YUV 0x5ef3fc01a1325fe9          /* rgb2yuv */
#define RGBSPLIT 0xd4c943cba60c270b         /* rgbsplit */
#define GAUSBLUR 0xaf8de3558a198d7a         /* gausblur */
#define BARRELDISTORT 0xfb8d958c0930c70a    /* barreldistort */
#define HORIZONTAL_CLAMP 0xb98672f1d40f31af /* horizontalclamp */
#define VERTICAL_CLAMP 0x63e452a2f47da7e8   /* verticalclamp */

#define MAX_SUBROUTINE_PASSES 2
#define NUM_SUBROUTINES 11
#define NUM_FILTERS 10

using namespace hans;

struct Filter {
  const uint16_t passes;
  const uint16_t subroutines[MAX_SUBROUTINE_PASSES];
};

struct FilterInfo {
  const hash names[NUM_FILTERS];
  const Filter filters[NUM_FILTERS];
};

// clang-format off
static constexpr char const* SUBROUTINES[] = {
  "passthrough_filter",
  "pixelate_filter",
  "greyscale_filter",
  "invert_filter",
  "rgb_to_yuv_filter",
  "rgbsplit_filter",
  "barrel_distort_filter",
  "gaus_filter_1",
  "gaus_filter_2",
  "horizontal_clamp_filter",
  "vertical_clamp_filter",
};

static constexpr const FilterInfo FILTER_INFO = {
  {
    PASSTHROUGH,
    PIXELATE,
    GREYSCALE,
    INVERT,
    RGB2YUV,
    RGBSPLIT,
    BARRELDISTORT,
    GAUSBLUR,
    HORIZONTAL_CLAMP,
    VERTICAL_CLAMP,
  },
  {
    {1, {0}},
    {1, {1}},
    {1, {2}},
    {1, {3}},
    {1, {4}},
    {1, {5}},
    {1, {6}},
    {2, {7, 8}},
    {1, {9}},
    {1, {10}},
  }
};
// clang-format on

struct Uniforms {
  GLuint texture;
  GLuint center;
  GLuint resolution;
  GLuint amount;
  GLuint weights;
  GLuint subroutines[NUM_SUBROUTINES];
};

struct FilterState {
  Parameter amount;
  Parameter filter;
  graphics::FBO fbo;
  graphics::ShaderProgram program;
  Register inlet;
  Register outlet;
  Uniforms uniforms;
  GLuint vao;
  uint32_t texture;

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

float gauss(float x, float sigma2) {
  double two_pi = 6.283185307179586;
  double coeff = 1.0 / (two_pi * sigma2);
  double expon = -(x * x) / (2.0 * sigma2);
  return (float)(coeff * exp(expon));
}

class FilterObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::INLET, 1);
    configurator.request(IConfigurator::Resources::OUTLET, 1);
  }

  virtual void setup(context& ctx) override {
    state.filter = ctx.parameters.make(id, FILTER);
    state.amount = ctx.parameters.make(id, AMOUNT);
    state.fbo = ctx.fbos.make(id);
    state.inlet = ctx.registers.make(id, Register::Types::INLET, 0);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
    state.program = ctx.shaders.create(ctx.shaders.create(SHADER_VERT),
                                       ctx.shaders.create(SHADER_FRAG));

    // Buffers
    float vertices[] = {-1, 1, -1, -1, 1, -1, 1, 1};
    int indices[] = {0, 1, 2, 2, 3, 0};

    glGenVertexArrays(1, &state.vao);
    glBindVertexArray(state.vao);

    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    GLuint ebo;
    glGenBuffers(1, &ebo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices,
                 GL_STATIC_DRAW);

    glBindVertexArray(state.vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);

    // Shaders
    glUseProgram(state.program.handle);

    auto pgm = state.program.handle;
    auto shdr = GL_FRAGMENT_SHADER;

    state.uniforms.resolution = glGetUniformLocation(pgm, "resolution");
    state.uniforms.center = glGetUniformLocation(pgm, "center");
    state.uniforms.texture = glGetUniformLocation(pgm, "image");
    state.uniforms.amount = glGetUniformLocation(pgm, "amount");
    state.uniforms.weights = glGetUniformLocation(pgm, "weights");

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);

    auto num = glGetSubroutineIndex(pgm, shdr, "FilterEffect");
    auto loc = glGetSubroutineUniformLocation(pgm, shdr, "FilterEffect");

    if (loc == GL_INVALID_INDEX) {
      throw std::runtime_error("gfx-filter, invalid subroutine uniform loc");
    }

    for (auto i = 0; i < NUM_SUBROUTINES; ++i) {
      auto name = SUBROUTINES[i];
      state.uniforms.subroutines[i] = glGetSubroutineIndex(pgm, shdr, name);
    }

    // Registers
    auto input = ctx.registers.read(state.inlet);
    state.texture = *static_cast<uint32_t*>(input);
    auto output = ctx.fbos.get_color_attachment(state.fbo, 0);
    ctx.registers.write(state.outlet, &output);
  }

  virtual void update(context& ctx) override {
  }

  virtual void draw(context& ctx) const override {
    auto amount = ctx.parameters.get(state.amount, 0);

    // Compute weights for gausian blur filter
    float weights[5];
    weights[0] = gauss(0, amount);
    auto sum = weights[0];
    for (auto i = 1; i < 5; i++) {
      weights[i] = gauss(i, amount);
      sum += 2 * weights[i];
    }
    for (auto i = 0; i < 5; i++) {
      weights[i] = weights[i] / sum;
    }

    glUseProgram(state.program.handle);

    // Activate & bind input texture
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, state.texture);
    glUniform1i(state.uniforms.texture, 0);

    // Calculate uniforms from input texture
    int width, height;
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);

    glUniform2f(state.uniforms.resolution, width, height);
    glUniform2f(state.uniforms.center, width / 2.f, height / 2.f);
    glUniform2f(state.uniforms.amount, amount,
                ctx.parameters.get(state.amount, 1));
    glUniform1fv(state.uniforms.weights, 5, weights);

    ctx.fbos.bind_fbo(state.fbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glBindVertexArray(state.vao);

    // Make the filter passes
    auto fltr = ctx.parameters.get(state.filter, 0);
    fltr = std::min<float>(std::max<float>(fltr, 0), NUM_FILTERS);
    auto info = FILTER_INFO.filters[std::lround(fltr)];

    for (auto i = 0; i < info.passes; ++i) {
      auto location = state.uniforms.subroutines[info.subroutines[i]];
      glUniformSubroutinesuiv(GL_FRAGMENT_SHADER, 1, &location);
      glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
    }
  }

 private:
  FilterState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<FilterState, FilterObject>("gfx-filter");
}
