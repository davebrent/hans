#ifndef GUI_STATE_H
#define GUI_STATE_H

#include <QObject>
#include <QStringList>
#include <QStringListModel>
#include <QTimer>
#include <hans/primitives.hpp>
#include <hans/user_config.hpp>
#include <vector>

namespace hans {
class Engine;
}

namespace gui {

struct ProgramChangeEvent {
  size_t program;
};

struct FrameChangeEvent {};

struct ParameterChangeEvent {
  size_t program;
  size_t object;
  hans::hash name;
  size_t index;
  size_t component;
  float value;
};

class State : public QObject {
  Q_OBJECT
 public:
  hans::user_data original;
  hans::EngineData compiled;
  State(const char* filepath);

  size_t active_program();
  QStringListModel* programs();
  QStringListModel* objects();
  std::vector<hans::user_parameter> parameters(size_t program, size_t object);

 signals:
  void programChange(ProgramChangeEvent event);
  void parameterChange(ParameterChangeEvent event);
  void frameChange(FrameChangeEvent event);

 public slots:
  void setEngine(hans::Engine* engine);
  void setProgram(size_t program);
  void setProgram(int program);
  void setParameter(const hans::Track track, float value, bool state);
  void setParameter(ParameterChangeEvent event);

 private slots:
  void nextFrame();

 private:
  hans::Engine* _engine;
  QTimer* _timer;
  QStringList _program_list;
  QStringListModel _programs;
  QStringList _objects_list;
  QStringListModel _objects;
  QStringList _parameters_list;
  QStringListModel _parameters;
  size_t _program;
  size_t _object;
};

} // namespace gui

#endif // GUI_STATE_H
