#include "./Image.hpp"
#include <FreeImage.h>
#include <iostream>

using namespace hans;
using namespace hans::media;

bool image::encode(const char* filepath, const common::Frame& frame) {
  auto channels = frame.channels();
  auto bitdepth = channels * 8;
  auto pitch = channels * frame.width;

  auto image = FreeImage_ConvertFromRawBits(
      frame.buffer, frame.width, frame.height, pitch, bitdepth, 0xff0000,
      0x00ff00, 0x0000ff, false);

  FreeImage_Save(FIF_PNG, image, filepath, 0);
  FreeImage_Unload(image);
  return true;
}