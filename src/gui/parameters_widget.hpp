#ifndef GUI_PARAMETERS_WIDGET_H
#define GUI_PARAMETERS_WIDGET_H

#include <QComboBox>
#include <QDoubleSpinBox>
#include <QSlider>
#include <QStackedWidget>
#include <QVBoxLayout>
#include <QWidget>
#include <hans/user_config.hpp>
#include <vector>
#include "state.hpp"

namespace gui {

class ParameterWidget : public QWidget {
  Q_OBJECT
 public:
  ParameterWidget(ParameterChangeEvent event, hans::user_parameter parameter,
                  State& state, QWidget* parent = 0);

 public slots:
  void handleSliderChanged(int slider);
  void handleSpinnerChanged(int spinner);

 private:
  State& _state;
  ParameterChangeEvent _event;
  std::vector<QDoubleSpinBox*> _spinners;
  std::vector<QSlider*> _sliders;
  hans::user_parameter _parameter;
};

class ObjectParametersWidget : public QWidget {
  Q_OBJECT
 public:
  ObjectParametersWidget(size_t program, size_t object, State& state,
                         QWidget* parent = 0);

 private:
  State& _state;
  size_t _program;
  size_t _object;
};

class ParametersWidget : public QWidget {
  Q_OBJECT
 public:
  ParametersWidget(State& state, QWidget* parent = 0);
  void handleProgramChange(ProgramChangeEvent event);

 public slots:
  void setObject(int object);

 private:
  State& _state;
  QComboBox* _objects;
  size_t _program;
  QStackedWidget* _stack;
  std::vector<size_t> _programs;
  QVBoxLayout* _layout;
};

} // namespace gui

#endif // GUI_PARAMETERS_WIDGET_H
