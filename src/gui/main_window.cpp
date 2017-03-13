#include "main_window.hpp"
#include <QComboBox>
#include <QDesktopWidget>
#include <QPlainTextEdit>
#include <QTabWidget>
#include <QToolBar>
#include "engine_widget.hpp"
#include "state.hpp"

using namespace gui;

MainWindow::MainWindow(State& state, QWidget* parent)
    : QMainWindow(parent),
      _state(state),
      _buses(_state.compiled.settings.audio, 1),
      _task_thread(_task_queue) {
  auto toolbar = addToolBar(tr("File"));

  auto programs_view = new QComboBox(this);
  programs_view->setModel(_state.programs());
  programs_view->setCurrentIndex(0);
  toolbar->addWidget(programs_view);

  /*
  {
    auto icon = QIcon::fromTheme("media-playback-start");
    auto action = new QAction(icon, tr("&Play"), this);
    action->setShortcuts(QKeySequence::New);
    action->setStatusTip(tr("Create a new file"));
    toolbar->addAction(action);
  }

  {
    auto icon = QIcon::fromTheme("media-playback-stop");
    auto action = new QAction(icon, tr("&Stop"), this);
    action->setShortcuts(QKeySequence::New);
    action->setStatusTip(tr("Create a new file"));
    toolbar->addAction(action);
  }

  {
    auto icon = QIcon::fromTheme("media-record");
    auto action = new QAction(icon, tr("&Record"), this);
    action->setShortcuts(QKeySequence::New);
    action->setStatusTip(tr("Create a new file"));
    toolbar->addAction(action);
  }
  */

  auto root = new QWidget(this);
  auto layout = new QGridLayout();
  root->setLayout(layout);

  _window = new EngineWidget(_state, _buses, root);
  _sequencer = new Sequencer(_task_queue, _state.compiled.sequences);

  auto tabs = new QTabWidget(root);

  {
    _parameters = new ParametersWidget(_state, tabs);
    tabs->addTab(_parameters, QString(tr("Parameters")));
  }

  {
    auto log = new QPlainTextEdit(tabs);
    tabs->addTab(log, QString(tr("Log")));
  }

  layout->addWidget(_window, 0, 0, 4, 8);
  layout->addWidget(tabs, 0, 8, 4, 4);
  layout->setColumnStretch(0, 8);
  layout->setColumnStretch(8, 4);
  layout->setRowStretch(0, 4);
  setCentralWidget(root);
  resize(QDesktopWidget().availableGeometry(this).size() * 0.6);

  // clang-format off
  connect(programs_view, SIGNAL(activated(int)),
          &_state, SLOT(setProgram(int)));
  connect(_sequencer, SIGNAL(trackEvent(const hans::Track&, float, bool)),
          &_state, SLOT(setParameter(const hans::Track&, float, bool)));

  connect(&_state, SIGNAL(programChange(ProgramChangeEvent)),
          this, SLOT(handleProgramChange(ProgramChangeEvent)));
  connect(&_state, SIGNAL(parameterChange(ParameterChangeEvent)),
          this, SLOT(handleParameterChange(ParameterChangeEvent)));
  connect(&_state, SIGNAL(frameChange(FrameChangeEvent)),
          this, SLOT(handleFrameChange(FrameChangeEvent)));
  // clang-format on

  _state.setProgram(0);
  _task_thread.start();
  _sequencer->start();
}

MainWindow::~MainWindow() {
  _sequencer->stop();
  _sequencer->wait();
  _task_queue.stop();
  _task_thread.wait();
}

void MainWindow::handleProgramChange(ProgramChangeEvent event) {
  _parameters->handleProgramChange(event);
}

void MainWindow::handleParameterChange(ParameterChangeEvent event) {
}

void MainWindow::handleFrameChange(FrameChangeEvent event) {
  _window->update();
}
