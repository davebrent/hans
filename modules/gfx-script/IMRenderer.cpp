#include "./IMRenderer.hpp"
#include "hans/graphics/gl.h"
#define NANOVG_GL3_IMPLEMENTATION 1
#include <nanovg_gl.h>

IMRenderer::IMRenderer() {
  m_nvg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES | NVG_DEBUG);
  m_width = 0;
  m_height = 0;

  m_no_stroke = true;
  m_no_fill = false;

  m_fill = nvgRGBA(255, 255, 255, 255);
  m_stroke = nvgRGBA(0, 0, 0, 255);

  nvgFillColor(m_nvg, m_fill);
  nvgStrokeColor(m_nvg, m_stroke);
}

IMRenderer::~IMRenderer() {
  if (m_nvg != nullptr) {
    nvgDeleteGL3(m_nvg);
  }
}

void IMRenderer::destroy() {
  nvgDeleteGL3(m_nvg);
  m_nvg = nullptr;
}

void IMRenderer::set_script_data(script_data* data) {
  m_script = data;
}

script_data* IMRenderer::get_script_data() {
  return m_script;
}

void IMRenderer::begin_frame() {
  nvgBeginFrame(m_nvg, m_width, m_height, m_width / m_height);
}

void IMRenderer::end_frame() {
  nvgEndFrame(m_nvg);
}

void IMRenderer::size(float width, float height) {
  m_width = width;
  m_height = height;
}

void IMRenderer::background(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
  glClearColor((float)r / 255, (float)g / 255, (float)b / 255, (float)a / 255);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
}

void IMRenderer::save() {
  nvgSave(m_nvg);
}

void IMRenderer::reset() {
  nvgReset(m_nvg);
}

void IMRenderer::restore() {
  nvgRestore(m_nvg);
}

void IMRenderer::fill(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
  m_no_fill = false;
  m_fill = nvgRGBA(r, g, b, a);
  nvgFillColor(m_nvg, m_fill);
}

void IMRenderer::no_fill() {
  m_no_fill = true;
}

void IMRenderer::stroke(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
  m_no_stroke = false;
  m_stroke = nvgRGBA(r, g, b, a);
  nvgStrokeColor(m_nvg, m_stroke);
}

void IMRenderer::no_stroke() {
  m_no_stroke = true;
}

void IMRenderer::stroke_width(float width) {
  nvgStrokeWidth(m_nvg, width);
}

void IMRenderer::ellipse(float x, float y, float w, float h) {
  nvgBeginPath(m_nvg);
  nvgEllipse(m_nvg, x, y, w / 2, h / 2);
  if (!m_no_fill) {
    nvgFill(m_nvg);
  }
  if (!m_no_stroke) {
    nvgStroke(m_nvg);
  }
}

void IMRenderer::line(float x1, float y1, float x2, float y2) {
  nvgBeginPath(m_nvg);
  nvgMoveTo(m_nvg, x1, y1);
  nvgLineTo(m_nvg, x2, y2);
  nvgStroke(m_nvg);
}

void IMRenderer::rect(float x, float y, float w, float h) {
  nvgBeginPath(m_nvg);
  nvgRect(m_nvg, x, y, w, h);
  if (!m_no_fill) {
    nvgFill(m_nvg);
  }
  if (!m_no_stroke) {
    nvgStroke(m_nvg);
  }
}

void IMRenderer::translate(float x, float y) {
  nvgTranslate(m_nvg, x, y);
}

void IMRenderer::rotate(float angle) {
  nvgRotate(m_nvg, angle);
}

void IMRenderer::scale(float x, float y) {
  nvgScale(m_nvg, x, y);
}

void IMRenderer::load_font(const char* name, const char* filepath) {
  nvgCreateFont(m_nvg, name, filepath);
}

void IMRenderer::text_font(const char* name) {
  nvgFontFace(m_nvg, name);
}

void IMRenderer::text_size(float size) {
  nvgFontSize(m_nvg, size);
}

float IMRenderer::text_width(const char* text) {
  return 0;
}

void IMRenderer::text(float x, float y, const char* text) {
  nvgText(m_nvg, x, y, text, nullptr);
}
