#include "engine_widget.hpp"
#include <QOpenGLFunctions>
#include <QOpenGLFunctions_4_2_Core>

using namespace gui;

EngineWidget::EngineWidget(State& state, hans::AudioBuses& buses,
                           QWidget* parent)
    : QOpenGLWidget(parent), _state(state), _buses(buses) {
}

void EngineWidget::initializeGL() {
  _engine = new hans::Engine(_state.compiled, _buses);
  _state.setEngine(_engine);
}

void EngineWidget::resizeGL(int width, int height) {
  _width = width;
  _height = height;
  hans::Display window;
  window.width = width;
  window.height = height;
  window.fbo = defaultFramebufferObject();
  _engine->set_display(window);
}

void EngineWidget::paintGL() {
  auto width = _state.compiled.settings.graphics.width;
  auto height = _state.compiled.settings.graphics.height;

  auto ctx = QOpenGLContext::currentContext();
  auto f = ctx->versionFunctions<QOpenGLFunctions_4_2_Core>();

  f->glBindFramebuffer(GL_DRAW_FRAMEBUFFER, defaultFramebufferObject());
  f->glDisable(GL_SCISSOR_TEST);
  f->glViewport(0, 0, _width, _height);
  f->glClearColor(0.8, 0.8, 0.8, 1.0);
  f->glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
  f->glViewport(0, 0, width, height);

  _engine->tick_graphics();
}
