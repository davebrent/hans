#include <vpx/vp8dx.h>
#include <vpx/vpx_decoder.h>
#include <fstream>
#include "hans/engine/object.hpp"

#define SHADER_VERT 0x10cfa0bb3018a6fa /* video/shader/vertex */
#define SHADER_FRAG 0xd45361e24a044b9c /* video/shader/fragment */
#define FILEPATH 0xde7d375053208813    /* filepath */

using namespace hans;

// Video player for `.ivf` files (https://wiki.multimedia.cx/index.php/IVF)

class VideoDecoder {
 public:
  struct rational {
    int numerator;
    int denominator;
  };

  struct video_info {
    uint32_t width;
    uint32_t height;
    rational time_base;
    uint32_t frames;
  };

  VideoDecoder() {
    // `.ivf` files are containers for VP8 data, it cant be by definition VP9
    _interface = &vpx_codec_vp8_dx_algo;
    vpx_codec_dec_init(&_codec, _interface, nullptr, 0);
    _frame_buffer = static_cast<char*>(std::calloc(1, 4096 * 4));
  }

  ~VideoDecoder() {
    std::free(_frame_buffer);
    vpx_codec_destroy(&_codec);
  }

  video_info open(const char* filepath) {
    _stream.open(filepath, std::ios::binary);
    if (!_stream.good()) {
      throw std::runtime_error("Video file not found");
    }

    // `.ivf` files have a 32 byte file header
    char header[32];
    _stream.read(reinterpret_cast<char*>(&header), 32);

    auto signature = mem_get_le32(header);
    if (signature != 1179208516) {
      // The characters 'DKIF' as an integer
      throw std::runtime_error("Invalid .ivf file");
    }

    auto version = mem_get_le16(header + 4);
    if (version != 0) {
      throw std::runtime_error("Unsupported .ivf version");
    }

    auto header_size = mem_get_le16(header + 6);
    if (header_size != 32) {
      throw std::runtime_error("Unknown header type, size is not 32");
    }

    auto codec4cc = mem_get_le32(header + 8);
    if (codec4cc != 808996950) {
      // The characters 'VP80' as an integer
      throw std::runtime_error("Invalid codec, must be VP80");
    }

    video_info info;
    info.width = mem_get_le16(header + 12);
    info.height = mem_get_le16(header + 14);
    info.time_base.numerator = mem_get_le32(header + 16);
    info.time_base.denominator = mem_get_le32(header + 20);
    info.frames = mem_get_le32(header + 24);
    return info;
  }

  vpx_image_t* decode() {
    // Read the 12 byte frame header; to get the number of bytes in the frame
    char frame_header[12];
    _stream.read(reinterpret_cast<char*>(&frame_header), 12);

    // This check is from ivfdec.c
    auto frame_size = mem_get_le32(frame_header);
    if (frame_size > 256 * 1024 * 1024) {
      throw std::runtime_error("Read invalid size");
    }

    if (frame_size >= 4096 * 4) {
      throw std::runtime_error("Video resize framebuffer");
    }

    _stream.read(_frame_buffer, frame_size);
    auto buffer = reinterpret_cast<unsigned char*>(_frame_buffer);

    // XXX: This bit can take up to 10ms++. Is very "bubbly". Maybe it should
    //      be moved to another thread?
    auto err = vpx_codec_decode(&_codec, buffer, frame_size, nullptr, 0);
    if (err) {
      std::cerr << "Failed to decode frame. error: "
                << vpx_codec_err_to_string(err) << std::endl;
      throw std::runtime_error("Failed to decode frame");
    }

    // Each call to 'decode' may produce multiple output frames...
    vpx_codec_iter_t iter = nullptr;
    vpx_image_t* img = vpx_codec_get_frame(&_codec, &iter);
    // But, is this really possible with a `.ivf` file? Where the format is
    // broken down into frames?
    if (vpx_codec_get_frame(&_codec, &iter) != nullptr) {
      throw std::runtime_error("Video decoded more than one frame");
    }

    return img;
  }

 private:
  uint16_t mem_get_le16(const void* data) {
    uint16_t val;
    const uint8_t* mem = (const uint8_t*)data;
    val = mem[1] << 8;
    val |= mem[0];
    return val;
  }

  uint32_t mem_get_le32(const void* data) {
    uint32_t val;
    const uint8_t* mem = (const uint8_t*)data;
    val = mem[3] << 24;
    val |= mem[2] << 16;
    val |= mem[1] << 8;
    val |= mem[0];
    return val;
  }

  char* _frame_buffer;
  vpx_codec_ctx_t _codec;
  vpx_codec_iface_t* _interface;
  std::ifstream _stream;
};

struct VideoState {
  graphics::FBO fbo;
  uint32_t planes[3]; // textures
  Register outlet;
  GLuint vao;
  graphics::ShaderProgram program;
  GLuint plane_uniforms[3];
  uint8_t* image_buffer;
  hash filepath = 0;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(filepath);
  }
};

class VideoObject : protected engine::GraphicsObject {
  friend class hans::engine::PluginManager;

 public:
  using engine::GraphicsObject::GraphicsObject;
  virtual void create(engine::Configurator& patcher) override;
  virtual void setup(engine::context& ctx) override;
  virtual void update(engine::context& ctx) override;
  virtual void draw(engine::context& ctx) const override;

 private:
  VideoState state;
  VideoDecoder _decoder;
};

void VideoObject::create(engine::Configurator& configurator) {
  configurator.request(engine::Configurator::Resources::OUTLET, 1);
  for (const auto& arg : configurator.arguments()) {
    if (arg.name == FILEPATH && arg.type == Argument::Types::STRING) {
      state.filepath = arg.string;
    }
  }

  if (!state.filepath) {
    configurator.missing("filepath");
  }
}

void VideoObject::setup(engine::context& ctx) {
  auto info = _decoder.open(ctx.strings.lookup(state.filepath));

  // Hans
  state.fbo = ctx.fbos.make(id);
  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.program = ctx.shaders.create(ctx.shaders.create(SHADER_VERT),
                                     ctx.shaders.create(SHADER_FRAG));

  state.image_buffer = new uint8_t[info.width * info.height];

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

  glUseProgram(state.program.handle);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(0);

  // Textures
  glGenTextures(3, reinterpret_cast<GLuint*>(&state.planes));
  for (auto i = 0; i < 3; ++i) {
    auto width = (i == 0) ? info.width : info.width / 2;
    auto height = (i == 0) ? info.height : info.height / 2;

    glBindTexture(GL_TEXTURE_2D, state.planes[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, width, height, 0, GL_RED,
                 GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  }

  // Uniforms
  auto pgm = state.program.handle;
  state.plane_uniforms[0] = glGetUniformLocation(pgm, "y_plane");
  state.plane_uniforms[1] = glGetUniformLocation(pgm, "u_plane");
  state.plane_uniforms[2] = glGetUniformLocation(pgm, "v_plane");

  // Registers
  auto output = ctx.fbos.get_color_attachment(state.fbo, 0);
  ctx.registers.write(state.outlet, &output);
}

// taken from tools_common.c
int vpx_img_plane_width(const vpx_image_t* img, int plane) {
  if (plane > 0 && img->x_chroma_shift > 0) {
    return (img->d_w + 1) >> img->x_chroma_shift;
  } else {
    return img->d_w;
  }
}

int vpx_img_plane_height(const vpx_image_t* img, int plane) {
  if (plane > 0 && img->y_chroma_shift > 0) {
    return (img->d_h + 1) >> img->y_chroma_shift;
  } else {
    return img->d_h;
  }
}

void VideoObject::update(engine::context& ctx) {
  auto img = _decoder.decode();

  // YV12 -> RGB
  for (auto i = 0; i < 3; ++i) {
    auto width = vpx_img_plane_width(img, i) *
                 ((img->fmt & VPX_IMG_FMT_HIGHBITDEPTH) ? 2 : 1);
    auto height = vpx_img_plane_height(img, i);

    // Data in planes is not stored contiguously
    auto in = img->planes[i];
    auto stride = img->stride[i];
    auto out = state.image_buffer;
    for (auto y = 0; y < height; ++y) {
      std::memcpy(out, in, sizeof(uint8_t) * width);
      out += width;
      in += stride;
    }

    glBindTexture(GL_TEXTURE_2D, state.planes[i]);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_RED,
                    GL_UNSIGNED_BYTE, state.image_buffer);
  }
}

void VideoObject::draw(engine::context& ctx) const {
  glUseProgram(state.program.handle);

  for (auto i = 0; i < 3; ++i) {
    glActiveTexture(GL_TEXTURE0 + i);
    glBindTexture(GL_TEXTURE_2D, state.planes[i]);
    glUniform1i(state.plane_uniforms[i], i);
  }

  ctx.fbos.bind_fbo(state.fbo);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

HANS_PLUGIN_INIT(engine::PluginManager* manager) {
  manager->add_object<VideoState, VideoObject>("gfx-video");
}
