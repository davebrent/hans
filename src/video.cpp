#include "hans/video.hpp"
#include <cassert>
#include <iostream>

using namespace hans;

VideoEncoder::VideoEncoder(const char* path, uint16_t width, uint16_t height) {
  using namespace std;

  m_width = width;
  m_height = height;
  m_capacity = m_width * m_height * 3;

  m_temp_row = static_cast<unsigned char*>(calloc(width, sizeof(char) * 3));
  m_frame = static_cast<unsigned char*>(calloc(m_capacity, sizeof(char)));
  m_plane = calloc(width * height, sizeof(char));

  m_stream.open(path, ofstream::out | ofstream::binary);
  m_stream << "YUV4MPEG2 "
           << "W" << width << " "
           << "H" << height << " "
           << "F60:1 "
           << "A1:1 "
           << "C444\n";
}

VideoEncoder::~VideoEncoder() {
  if (m_stream.is_open()) {
    m_stream.close();
  }

  std::free(m_frame);
  std::free(m_plane);
}

static void BGR_to_YCbCr(uint8_t* BGR, uint8_t* YCbCr) {
  float r = BGR[2] / 255.f, g = BGR[1] / 255.f, b = BGR[0] / 255.f;

  // ITU-R BT.709
  float Kr = 0.2126;
  float Kg = 0.587;
  float Kb = 0.0722;

  YCbCr[0] = ((Kr * r + Kg * g + Kb * b) * 235.f) + 16;
  YCbCr[1] = ((-0.168736 * r - 0.331264 * g + 0.5 * b) * 240.f) + 128;
  YCbCr[2] = ((0.5 * r - 0.418688 * g - 0.081312 * b) * 240.f) + 128;
}

static void convert_image(unsigned char* output, unsigned char* input,
                          uint16_t width, uint16_t height) {
  auto input_bytes_per_pixel = 4 * sizeof(char);  // rgba
  auto output_bytes_per_pixel = 3 * sizeof(char); // YCbCr

  for (auto i = 0; i < width * height; ++i) {
    BGR_to_YCbCr(input, output);
    input += input_bytes_per_pixel;
    output += output_bytes_per_pixel;
  }
}

static void flip_vertically(unsigned char* pixels, unsigned char* row,
                            uint16_t width, uint16_t height) {
  auto bytes_per_pixel = 3 * sizeof(char);
  auto stride = width * bytes_per_pixel;
  auto low = pixels;
  auto high = &pixels[(height - 1) * stride];

  for (; low < high; low += stride, high -= stride) {
    std::memcpy(row, low, stride);
    std::memcpy(low, high, stride);
    std::memcpy(high, row, stride);
  }
}

bool VideoEncoder::encode(const Frame& frame) {
  assert(frame.width == m_width);
  assert(frame.height == m_height);

  convert_image(m_frame, frame.buffer, m_width, m_height);
  flip_vertically(m_frame, m_temp_row, m_width, m_height);

  auto out_frame = static_cast<uint8_t*>(m_frame);
  auto plane = static_cast<uint8_t*>(m_plane);
  auto len = m_width * m_height;

  m_stream << "FRAME\n";

  for (auto f = 0; f < 3; ++f) {
    for (auto i = 0; i < len; ++i) {
      plane[i] = out_frame[(i * 3) + f];
    }

    m_stream.write(static_cast<char*>(m_plane), len * sizeof(char));
  }

  return true;
}

bool VideoEncoder::close() {
  m_stream.flush();
  m_stream.close();
  return true;
}
