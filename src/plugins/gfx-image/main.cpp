#include <webp/decode.h>
#include <fstream>
#include <stdexcept>
#include "hans/object.hpp"

#define FILEPATH 0xde7d375053208813 /* filepath */
#define IMAGE 0x284126856b22844e    /* image */

using namespace hans;

struct ImageState {
  Parameter index;
  Register outlet;
  std::vector<hash> filepaths;
  std::vector<GLuint> textures;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(filepaths);
  }
};

class ImageObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::OUTLET, 1);
    for (const auto& arg : configurator.arguments()) {
      if (arg.name == FILEPATH && arg.type == Argument::Types::STRING) {
        state.filepaths.push_back(arg.string);
      }
    }

    if (!state.filepaths.size()) {
      configurator.missing("filepath");
    }
  }

  virtual void setup(context& ctx) override {
    auto len = state.filepaths.size();
    state.textures.assign(len, 0);
    glGenTextures(len, state.textures.data());

    auto index = 0;
    for (const auto& filepath : state.filepaths) {
      std::ifstream is(ctx.strings.lookup(filepath),
                       std::ios::binary | std::ios::ate);

      if (!is.good()) {
        throw std::runtime_error("File does not exist");
      }

      auto size = is.tellg();
      is.seekg(0, std::ios::beg);
      std::vector<uint8_t> buff(size);
      is.read(reinterpret_cast<char*>(buff.data()), size);

      int width, height;
      if (WebPGetInfo(buff.data(), size, &width, &height) == false) {
        throw std::runtime_error("Image is not a webp image");
      }

      auto rgba = WebPDecodeRGBA(buff.data(), size, &width, &height);
      glBindTexture(GL_TEXTURE_2D, state.textures.at(index));
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA,
                   GL_UNSIGNED_BYTE, rgba);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      WebPFree(rgba);
      index++;
    }

    state.index = ctx.parameters.make(id, IMAGE);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  }

  virtual void update(context& ctx) override {
  }

  virtual void draw(context& ctx) const override {
    auto index = ctx.parameters.get(state.index, 0);
    index = std::round(index);
    index = std::min<int>(std::max<int>(index, 0), state.textures.size() - 1);
    ctx.registers.write(state.outlet, state.textures.at(index));
  }

 private:
  ImageState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<ImageState, ImageObject>("gfx-image");
}
