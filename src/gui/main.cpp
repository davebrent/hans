#include <QApplication>
#include <QSurfaceFormat>
#include <hans/primitives.hpp>
#include <iostream>
#include "main_window.hpp"
#include "state.hpp"

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "usage: " << argv[0] << " <config>" << std::endl;
    return 0;
  }

  qRegisterMetaType<hans::Track>("hans::Track");

  QApplication app(argc, argv);

  QSurfaceFormat format;
  format.setVersion(4, 2);
  format.setProfile(QSurfaceFormat::CoreProfile);
  format.setDepthBufferSize(24);
  format.setStencilBufferSize(8);
  QSurfaceFormat::setDefaultFormat(format);

  gui::State state(argv[1]);
  gui::MainWindow window(state);
  window.show();

  return app.exec();
}
