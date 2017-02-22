#include "hans/video.hpp"
#include <vpx/vp8cx.h>
#include <cstring>
#include <stdexcept>

using namespace hans;

static const int VP8_FOURCC = 0x30385056;

static void mem_put_le16(void* vmem, int val) {
  auto mem = static_cast<unsigned char*>(vmem);
  mem[0] = (val >> 0) & 0xff;
  mem[1] = (val >> 8) & 0xff;
}

static void mem_put_le32(void* vmem, int val) {
  auto mem = static_cast<unsigned char*>(vmem);
  mem[0] = (val >> 0) & 0xff;
  mem[1] = (val >> 8) & 0xff;
  mem[2] = (val >> 16) & 0xff;
  mem[3] = (val >> 24) & 0xff;
}

static void ivf_write_file_header(std::ostream& os,
                                  const vpx_codec_enc_cfg_t& cfg,
                                  int frame_cnt) {
  char header[32];
  header[0] = 'D';
  header[1] = 'K';
  header[2] = 'I';
  header[3] = 'F';
  mem_put_le16(header + 4, 0);                   // version
  mem_put_le16(header + 6, 32);                  // header size
  mem_put_le32(header + 8, VP8_FOURCC);          // fourcc
  mem_put_le16(header + 12, cfg.g_w);            // width
  mem_put_le16(header + 14, cfg.g_h);            // height
  mem_put_le32(header + 16, cfg.g_timebase.den); // rate
  mem_put_le32(header + 20, cfg.g_timebase.num); // scale
  mem_put_le32(header + 24, frame_cnt);          // length
  mem_put_le32(header + 28, 0);                  // unused
  os.write(reinterpret_cast<char*>(&header), 32);
}

static void ivf_write_frame_header(std::ostream& os, int64_t pts,
                                   size_t frame_size) {
  char header[12];
  mem_put_le32(header, (int)frame_size);
  mem_put_le32(header + 4, (int)(pts & 0xFFFFFFFF));
  mem_put_le32(header + 8, (int)(pts >> 32));
  os.write(reinterpret_cast<char*>(&header), 12);
}

static void flip_vertically(unsigned char* pixels, unsigned char* row,
                            uint16_t width, uint16_t height) {
  auto bytes_per_pixel = 4 * sizeof(char);
  auto stride = width * bytes_per_pixel;
  auto low = pixels;
  auto high = &pixels[(height - 1) * stride];

  for (; low < high; low += stride, high -= stride) {
    std::memcpy(row, low, stride);
    std::memcpy(low, high, stride);
    std::memcpy(high, row, stride);
  }
}

VideoEncoder::VideoEncoder(std::ostream& os, size_t frames, uint16_t width,
                           uint16_t height)
    : _frameno(0), _os(os) {
  _temp_row = static_cast<unsigned char*>(calloc(width, sizeof(char) * 4));
  auto iface = vpx_codec_vp8_cx();

  vpx_codec_enc_cfg_t cfg;
  if (vpx_codec_enc_config_default(iface, &cfg, 0) != VPX_CODEC_OK) {
    throw std::runtime_error("Video failed to get default config");
  }

  cfg.g_w = width;
  cfg.g_h = height;
  cfg.g_timebase.num = 1;
  cfg.g_timebase.den = 60;

  if (vpx_codec_enc_init(&_codec, iface, &cfg, 0) != VPX_CODEC_OK) {
    throw std::runtime_error("Video failed to initialize codec");
  }

  if (!vpx_img_alloc(&_img, VPX_IMG_FMT_I420, width, height, 1)) {
    throw std::runtime_error("Video failed allocate image");
  }

  ivf_write_file_header(_os, cfg, frames);
}

VideoEncoder::~VideoEncoder() {
  vpx_codec_encode(&_codec, nullptr, _frameno, 1, 0, VPX_DL_BEST_QUALITY);
  vpx_codec_iter_t iter = nullptr;

  while (true) {
    auto pkt = vpx_codec_get_cx_data(&_codec, &iter);
    if (pkt == nullptr) {
      break;
    }

    if (pkt->kind == VPX_CODEC_CX_FRAME_PKT) {
      auto buff = pkt->data.frame.buf;
      auto size = pkt->data.frame.sz;
      ivf_write_frame_header(_os, pkt->data.frame.pts, size);
      _os.write(reinterpret_cast<char*>(buff), size);
    }
  }

  vpx_img_free(&_img);
  vpx_codec_destroy(&_codec);
}

void VideoEncoder::encode(const Frame& frame) {
  flip_vertically(frame.buffer, _temp_row, frame.width, frame.height);

  auto Y = _img.planes[0];
  auto V = _img.planes[1];
  auto U = _img.planes[2];

  // ITU-R BT.709
  auto Kr = 0.2126;
  auto Kg = 0.587;
  auto Kb = 0.0722;

  for (auto i = 0; i < frame.width * frame.height * 4; i += 4) {
    auto b = frame.buffer[i + 0] / 255.f;
    auto g = frame.buffer[i + 1] / 255.f;
    auto r = frame.buffer[i + 2] / 255.f;
    *Y++ = ((Kr * r + Kg * g + Kb * b) * 235.f) + 16;
  }

  for (auto r = 0; r < frame.height; r += 2) {
    auto o = r * frame.width * 4;

    for (auto i = 0; i < frame.width * 4; i += 8) {
      auto row = frame.width * 4;

      auto b1 = frame.buffer[o + i + 0];
      auto g1 = frame.buffer[o + i + 1];
      auto r1 = frame.buffer[o + i + 2];

      auto b2 = frame.buffer[o + i + 4];
      auto g2 = frame.buffer[o + i + 5];
      auto r2 = frame.buffer[o + i + 6];

      auto b3 = frame.buffer[o + i + row + 0];
      auto g3 = frame.buffer[o + i + row + 1];
      auto r3 = frame.buffer[o + i + row + 2];

      auto b4 = frame.buffer[o + i + row + 4];
      auto g4 = frame.buffer[o + i + row + 5];
      auto r4 = frame.buffer[o + i + row + 6];

      // XXX: Look at better ways of chroma subsampling
      auto r = ((r1 + r2 + r3 + r4) / 4.f) / 255.f;
      auto g = ((g1 + g2 + g3 + g4) / 4.f) / 255.f;
      auto b = ((b1 + b2 + b3 + b4) / 4.f) / 255.f;

      *V++ = ((-0.168736 * r - 0.331264 * g + 0.5 * b) * 240.f) + 128;
      *U++ = ((0.5 * r - 0.418688 * g - 0.081312 * b) * 240.f) + 128;
    }
  }

  auto pts = _frameno;
  _frameno++;

  if (vpx_codec_encode(&_codec, &_img, pts, 1, 0, VPX_DL_BEST_QUALITY) !=
      VPX_CODEC_OK) {
    throw std::runtime_error("Video failed to encode frame data");
  }

  vpx_codec_iter_t iter = nullptr;
  while (true) {
    auto pkt = vpx_codec_get_cx_data(&_codec, &iter);
    if (pkt == nullptr) {
      break;
    }

    if (pkt->kind == VPX_CODEC_CX_FRAME_PKT) {
      auto buff = pkt->data.frame.buf;
      auto size = pkt->data.frame.sz;
      ivf_write_frame_header(_os, pkt->data.frame.pts, size);
      _os.write(reinterpret_cast<char*>(buff), size);
    }
  }
}
