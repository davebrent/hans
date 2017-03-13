#ifndef GUI_MAIN_WINDOW_H
#define GUI_MAIN_WINDOW_H

#include <QMainWindow>
#include <QTimer>
#include <hans/primitives.hpp>
#include <hans/tasks.hpp>
#include "engine_widget.hpp"
#include "parameters_widget.hpp"
#include "sequencer.hpp"
#include "state.hpp"
#include "worker_thread.hpp"

namespace gui {

class MainWindow : public QMainWindow {
  Q_OBJECT

 public:
  MainWindow(State& state, QWidget* parent = 0);
  ~MainWindow();

 public slots:
  void handleProgramChange(ProgramChangeEvent event);
  void handleParameterChange(ParameterChangeEvent event);
  void handleFrameChange(FrameChangeEvent event);

 private:
  State& _state;
  hans::AudioBuses _buses;
  EngineWidget* _window;
  ParametersWidget* _parameters;
  Sequencer* _sequencer;
  hans::TaskQueue _task_queue;
  WorkerThread _task_thread;
  QTimer* _timer;
};

} // namespace gui

#endif // GUI_MAIN_WINDOW_H
