#ifndef GUI_ENGINE_WIDGET_H
#define GUI_ENGINE_WIDGET_H

#define HANS_SKIP_GL 1

#include <QOpenGLWidget>
#include <hans/audio_buses.hpp>
#include <hans/engine.hpp>
#include "state.hpp"

namespace gui {

class EngineWidget : public QOpenGLWidget {
  Q_OBJECT
 public:
  EngineWidget(State& state, hans::AudioBuses& buses, QWidget* parent = 0);
  void initializeGL() override;
  void resizeGL(int width, int height) override;
  void paintGL() override;

 private:
  State& _state;
  hans::AudioBuses& _buses;
  hans::Engine* _engine = nullptr;
  int _width;
  int _height;
};

} // namespace gui

#endif // GUI_ENGINE_WIDGET_H
