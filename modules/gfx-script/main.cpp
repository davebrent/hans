#include <libguile.h>
#include <cstdlib>
#include <iostream>
#include "./IMRenderer.hpp"
#include "./types.hpp"
#include "hans/engine/object.hpp"

#define ARG_PATH 0xae70259f6415b584

using namespace hans;

static SCM size(SCM w, SCM h) {
  auto& renderer = IMRenderer::get_instance();
  auto data = renderer.get_script_data();
  data->width = scm_to_double(w);
  data->height = scm_to_double(h);
  renderer.size(data->width, data->height);
  return SCM_BOOL_T;
}

static SCM background(SCM r, SCM g, SCM b, SCM a) {
  IMRenderer::get_instance().background(scm_to_int(r), scm_to_int(g),
                                        scm_to_int(b), scm_to_int(a));
  return SCM_BOOL_T;
}

static SCM save() {
  IMRenderer::get_instance().save();
  return SCM_BOOL_T;
}

static SCM reset() {
  IMRenderer::get_instance().reset();
  return SCM_BOOL_T;
}

static SCM restore() {
  IMRenderer::get_instance().restore();
  return SCM_BOOL_T;
}

static SCM fill(SCM r, SCM g, SCM b, SCM a) {
  IMRenderer::get_instance().fill(scm_to_double(r), scm_to_double(g),
                                  scm_to_double(b), scm_to_double(a));
  return SCM_BOOL_T;
}

static SCM no_fill() {
  IMRenderer::get_instance().no_fill();
  return SCM_BOOL_T;
}

static SCM stroke(SCM r, SCM g, SCM b, SCM a) {
  IMRenderer::get_instance().stroke(scm_to_double(r), scm_to_double(g),
                                    scm_to_double(b), scm_to_double(a));
  return SCM_BOOL_T;
}

static SCM no_stroke() {
  IMRenderer::get_instance().no_stroke();
  return SCM_BOOL_T;
}

static SCM stroke_width(SCM width) {
  IMRenderer::get_instance().stroke_width(scm_to_double(width));
  return SCM_BOOL_T;
}

static SCM ellipse(SCM cx, SCM cy, SCM width, SCM height) {
  IMRenderer::get_instance().ellipse(scm_to_double(cx), scm_to_double(cy),
                                     scm_to_double(width),
                                     scm_to_double(height));
  return SCM_BOOL_T;
}

static SCM line(SCM x1, SCM y1, SCM x2, SCM y2) {
  IMRenderer::get_instance().line(scm_to_double(x1), scm_to_double(y1),
                                  scm_to_double(x2), scm_to_double(y2));
  return SCM_BOOL_T;
}

static SCM triangle(SCM x1, SCM y1, SCM x2, SCM y2, SCM x3, SCM y3) {
  IMRenderer::get_instance().triangle(scm_to_double(x1), scm_to_double(y1),
                                      scm_to_double(x2), scm_to_double(y2),
                                      scm_to_double(x3), scm_to_double(y3));
  return SCM_BOOL_T;
}

static SCM quad(SCM x1, SCM y1, SCM x2, SCM y2, SCM x3, SCM y3, SCM x4,
                SCM y4) {
  IMRenderer::get_instance().quad(scm_to_double(x1), scm_to_double(y1),
                                  scm_to_double(x2), scm_to_double(y2),
                                  scm_to_double(x3), scm_to_double(y3),
                                  scm_to_double(x4), scm_to_double(y4));
  return SCM_BOOL_T;
}

static SCM rect(SCM x, SCM y, SCM w, SCM h) {
  IMRenderer::get_instance().rect(scm_to_double(x), scm_to_double(y),
                                  scm_to_double(w), scm_to_double(h));
  return SCM_BOOL_T;
}

static SCM translate(SCM x, SCM y) {
  IMRenderer::get_instance().translate(scm_to_double(x), scm_to_double(y));
  return SCM_BOOL_T;
}

static SCM rotate(SCM angle) {
  IMRenderer::get_instance().rotate(scm_to_double(angle));
  return SCM_BOOL_T;
}

static SCM scale(SCM x, SCM y) {
  IMRenderer::get_instance().scale(scm_to_double(x), scm_to_double(y));
  return SCM_BOOL_T;
}

static SCM load_font(SCM name, SCM path) {
  auto name_str = scm_to_locale_string(name);
  auto path_str = scm_to_locale_string(path);
  IMRenderer::get_instance().load_font(name_str, path_str);
  std::free(name_str);
  std::free(path_str);
  return SCM_BOOL_T;
}

static SCM text_font(SCM name) {
  auto name_str = scm_to_locale_string(name);
  IMRenderer::get_instance().text_font(name_str);
  std::free(name_str);
  return SCM_BOOL_T;
}

static SCM text_size(SCM size) {
  IMRenderer::get_instance().text_size(scm_to_double(size));
  return SCM_BOOL_T;
}

static SCM text_width(SCM text) {
  auto text_str = scm_to_locale_string(text);
  auto res = scm_from_double(IMRenderer::get_instance().text_width(text_str));
  std::free(text_str);
  return res;
}

static SCM text(SCM x, SCM y, SCM text) {
  auto text_str = scm_to_locale_string(text);
  IMRenderer::get_instance().text(scm_to_double(x), scm_to_double(y), text_str);
  std::free(text_str);
  return SCM_BOOL_T;
}

static SCM draw(SCM proc) {
  auto data = IMRenderer::get_instance().get_script_data();
  data->draw = proc;
  return SCM_BOOL_T;
}

void script_new(hans_constructor_api* api, void* buffer, size_t size) {
  auto data = static_cast<script_data*>(buffer);

  uint8_t num_inlets = 1;
  uint8_t num_outlets = 1;

  api->request_resource(api, HANS_INLET, &num_inlets);
  api->request_resource(api, HANS_OUTLET, &num_outlets);

  auto args = api->get_arguments(api);
  for (int i = 0; i < args.length; ++i) {
    if (args.data[i].type == HANS_STRING && args.data[i].name == ARG_PATH) {
      data->path = args.data[i].string;
    }
  }
}

void script_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<script_data*>(self->data);

  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->fbo = api->fbos->make(self->id);

  auto texture = api->fbos->get_color_attachment(data->fbo, 0);
  api->registers->write(data->outlet, &texture);

  scm_c_define_gsubr("size", 2, 0, 0, (scm_t_subr)size);
  scm_c_define_gsubr("save", 0, 0, 0, (scm_t_subr)save);
  scm_c_define_gsubr("reset", 0, 0, 0, (scm_t_subr)reset);
  scm_c_define_gsubr("restore", 0, 0, 0, (scm_t_subr)restore);
  scm_c_define_gsubr("background", 4, 0, 0, (scm_t_subr)background);
  scm_c_define_gsubr("fill", 4, 0, 0, (scm_t_subr)fill);
  scm_c_define_gsubr("no-fill", 0, 0, 0, (scm_t_subr)no_fill);
  scm_c_define_gsubr("stroke", 4, 0, 0, (scm_t_subr)stroke);
  scm_c_define_gsubr("no-stroke", 0, 0, 0, (scm_t_subr)no_stroke);
  scm_c_define_gsubr("stroke-width", 1, 0, 0, (scm_t_subr)stroke_width);
  scm_c_define_gsubr("ellipse", 4, 0, 0, (scm_t_subr)ellipse);
  scm_c_define_gsubr("line", 4, 0, 0, (scm_t_subr)line);
  scm_c_define_gsubr("rect", 4, 0, 0, (scm_t_subr)rect);
  scm_c_define_gsubr("triangle", 6, 0, 0, (scm_t_subr)triangle);
  scm_c_define_gsubr("quad", 8, 0, 0, (scm_t_subr)quad);
  scm_c_define_gsubr("translate", 2, 0, 0, (scm_t_subr)translate);
  scm_c_define_gsubr("rotate", 1, 0, 0, (scm_t_subr)rotate);
  scm_c_define_gsubr("scale", 2, 0, 0, (scm_t_subr)scale);
  scm_c_define_gsubr("load-font", 2, 0, 0, (scm_t_subr)load_font);
  scm_c_define_gsubr("text-font", 1, 0, 0, (scm_t_subr)text_font);
  scm_c_define_gsubr("text-size", 1, 0, 0, (scm_t_subr)text_size);
  scm_c_define_gsubr("text-width", 1, 0, 0, (scm_t_subr)text_width);
  scm_c_define_gsubr("text", 3, 0, 0, (scm_t_subr)text);
  scm_c_define_gsubr("draw", 1, 0, 0, (scm_t_subr)draw);

  auto& renderer = IMRenderer::get_instance();
  renderer.set_script_data(data);
  scm_c_primitive_load(api->strings->lookup(data->path));
  renderer.set_script_data(nullptr);
}

void script_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<script_data*>(self->data);

  api->fbos->bind_fbo(data->fbo);

  auto& renderer = IMRenderer::get_instance();
  renderer.size(data->width, data->height);
  renderer.begin_frame();
  scm_call_0(data->draw);
  renderer.end_frame();
}

void script_destroy(void* instance) {
  IMRenderer::get_instance().destroy();
}

void script_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = script_setup;
  object->update = nullptr;
  object->draw = script_draw;
}

extern "C" {
void setup(hans_library_api* api) {
  api->register_object(api, "gfx-script", sizeof(script_data), script_new,
                       script_init, script_destroy);
}
}
