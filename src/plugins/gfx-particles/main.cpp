#include <Eigen/Dense>
#include <fstream>
#include "bytecode.hpp"
#include "hans/hasher.hpp"
#include "hans/object.hpp"
#include "virtual_machine.hpp"
#include "visualisers.hpp"

#define FILEPATH 0xde7d375053208813 /* filepath */
#define SETUP 0x8a4d34c9bd06d1f5    /* setup */
#define UPDATE 0xb09bdf90e8a56b76   /* update */

using namespace hans;

struct ParticlesState {
  Register inlet0;
  Register outlet;
  graphics::FBO fbo;
  hash filepath;
  vm::State state;
  bytecode::Program program;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(filepath);
  }
};

class ParticlesObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::INLET, 1);
    configurator.request(IConfigurator::Resources::OUTLET, 1);
    for (const auto& arg : configurator.arguments()) {
      if (arg.name == FILEPATH && arg.type == Argument::Types::STRING) {
        state.filepath = arg.string;
      }
    }
  }

  virtual void setup(context& ctx) override {
    state.fbo = ctx.fbos.make(id);
    state.inlet0 = ctx.registers.make(id, Register::Types::INLET, 0);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);

    state.state.visualiser_points.setup(ctx);

    auto filepath = ctx.strings.lookup(state.filepath);
    std::ifstream str(filepath);
    std::vector<std::string> tokens;

    state.state.seed = 0;
    bytecode::tokenize(tokens, str);
    bytecode::compile(state.program, tokens);
    vm::eval(state.state, state.program, SETUP);
    state.state.heap_frame_start = state.state.heap.size();
  }

  virtual void update(context& ctx) override {
    state.state.seed += 1;
    vm::eval(state.state, state.program, UPDATE);
    //vm::print_buffer(std::cout, state.state.dstack.back().buffer);
  }

  virtual void draw(context& ctx) const override {
    ctx.fbos.bind_fbo(state.fbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    for (const auto visualiser : state.state.draw_cmds) {
      switch (visualiser) {
      case VISUALISER_POINTS:
        state.state.visualiser_points.draw();
        break;
      }
    }

    auto texture = ctx.fbos.get_color_attachment(state.fbo, 0);
    ctx.registers.write(state.outlet, texture);
  }

 private:
  ParticlesState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<ParticlesState, ParticlesObject>("gfx-particles");
}

int main(int argc, char* argv[]) {
  std::vector<std::string> tokens;
  std::ifstream str(argv[2]);

  bytecode::Program program;
  bytecode::tokenize(tokens, str);
  bytecode::compile(program, tokens);
  bytecode::decompile(std::cout, program);
  return 0;
}
