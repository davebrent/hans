#ifndef HANS_GRAPHICS_IMRENDERER_H_
#define HANS_GRAPHICS_IMRENDERER_H_

#include <nanovg.h>
#include <cstdint>
#include "./types.hpp"

class IMRenderer {
 public:
  IMRenderer();
  ~IMRenderer();
  IMRenderer(IMRenderer const&) = delete;
  void operator=(IMRenderer const&) = delete;

  static IMRenderer& get_instance() {
    static IMRenderer instance;
    return instance;
  }

  void destroy();
  void set_script_state(ScriptState* data);
  ScriptState* get_script_state();

  bool should_loop();
  void begin_frame();
  void end_frame();

  void size(float width, float height);
  void background(uint8_t r, uint8_t g, uint8_t b, uint8_t a);
  void noloop();

  void save();
  void reset();
  void restore();

  void fill(uint8_t r, uint8_t g, uint8_t b, uint8_t a);
  void no_fill();
  void stroke(uint8_t r, uint8_t g, uint8_t b, uint8_t a);
  void no_stroke();
  void stroke_width(float width);

  void ellipse(float x, float y, float w, float h);
  void line(float x1, float y1, float x2, float y2);
  void rect(float x, float y, float w, float h);
  void triangle(float x1, float y1, float x2, float y2, float x3, float y3);
  void quad(float x1, float y1, float x2, float y2, float x3, float y3,
            float x4, float y4);

  void translate(float x, float y);
  void rotate(float angle);
  void scale(float x, float y);

  void load_font(const char* name, const char* filepath);
  void text_font(const char* name);
  void text_size(float size);
  float text_width(const char* text);
  void text(float x, float y, const char* text);

 private:
  NVGcontext* m_nvg;
  ScriptState* m_script = nullptr;

  float m_width;
  float m_height;

  bool m_no_stroke;
  bool m_no_fill;
  bool m_no_loop;

  NVGcolor m_fill;
  NVGcolor m_stroke;
};

#endif // HANS_GRAPHICS_IMRENDERER_H_
