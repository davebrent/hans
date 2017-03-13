#include "parameters_widget.hpp"
#include <QDebug>
#include <QFormLayout>
#include <QLabel>
#include <QScrollArea>
#include <QSignalMapper>
#include <QVBoxLayout>
#include <hans/hasher.hpp>
#include <hans/primitives.hpp>

using namespace gui;

ParameterWidget::ParameterWidget(ParameterChangeEvent event,
                                 hans::user_parameter parameter, State& state,
                                 QWidget* parent)
    : QWidget(parent), _state(state), _event(event), _parameter(parameter) {
  auto layout = new QHBoxLayout(this);
  setLayout(layout);
  auto mapper = new QSignalMapper(this);

  if (parameter.min.size() == 0 || parameter.max.size() == 0) {
    auto i = 0;
    for (const auto value : parameter.value) {
      auto spinner = new QDoubleSpinBox();
      _spinners.push_back(spinner);
      spinner->setRange(-65536, 65536);
      spinner->setValue(value);
      layout->addWidget(spinner);
      connect(spinner, SIGNAL(valueChanged(double)), mapper, SLOT(map()));
      mapper->setMapping(spinner, i);
      i++;
    }
    connect(mapper, SIGNAL(mapped(int)), this, SLOT(handleSpinnerChanged(int)));
  } else {
    auto i = 0;
    for (const auto value : parameter.value) {
      auto slider = new QSlider(Qt::Horizontal);
      _sliders.push_back(slider);
      slider->setValue(value);
      layout->addWidget(slider);
      connect(slider, SIGNAL(valueChanged(int)), mapper, SLOT(map()));
      mapper->setMapping(slider, i);
      i++;
    }
    connect(mapper, SIGNAL(mapped(int)), this, SLOT(handleSliderChanged(int)));
  }

  if (_parameter.step != 0) {
    for (const auto& widget : _spinners) {
      widget->setSingleStep(_parameter.step);
    }
  }

  layout->setContentsMargins(0, 0, 0, 0);
}

void ParameterWidget::handleSliderChanged(int slider) {
  auto value = _sliders.at(slider)->value() / 100.0;
  auto min = _parameter.min.at(slider);
  auto max = _parameter.max.at(slider);
  value = (value * (max - min)) + min;
  _event.component = slider;
  _event.value = value;
  _state.setParameter(_event);
}

void ParameterWidget::handleSpinnerChanged(int spinner) {
  _event.component = spinner;
  _event.value = _spinners.at(spinner)->value();
  _state.setParameter(_event);
}

ObjectParametersWidget::ObjectParametersWidget(size_t program, size_t object,
                                               State& state, QWidget* parent)
    : QWidget(parent), _state(state), _program(program), _object(object) {
  auto layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);

  auto params_widget = new QWidget(this);
  auto params_layout = new QFormLayout();
  params_widget->setLayout(params_layout);

  auto parameters = _state.parameters(_program, _object);
  auto i = 0;
  for (const auto& parameter : parameters) {
    ParameterChangeEvent event;
    event.program = _program;
    event.object = _object;
    event.name = hans::hasher(parameter.name.c_str());
    event.index = i;
    params_layout->addRow(new QLabel(QString::fromStdString(parameter.name)),
                          new ParameterWidget(event, parameter, _state, this));
    i++;
  }

  auto scrollarea = new QScrollArea(this);
  scrollarea->setWidgetResizable(true);
  scrollarea->setWidget(params_widget);
  layout->addWidget(scrollarea);
}

ParametersWidget::ParametersWidget(State& state, QWidget* parent)
    : QWidget(parent), _state(state) {
  auto label = new QLabel(tr("Object"), this);
  _layout = new QVBoxLayout(this);
  _objects = new QComboBox(this);
  _objects->setModel(_state.objects());

  auto form_widget = new QWidget(this);
  auto form_layout = new QFormLayout(form_widget);

  int l, t, r, b;
  form_layout->getContentsMargins(&l, &t, &r, &b);
  form_layout->setContentsMargins(0, t, 0, b);
  form_widget->setLayout(form_layout);
  form_layout->addRow(label, _objects);

  _stack = new QStackedWidget(this);

  auto index = 0;
  for (auto program = 0; program < state.original.programs.size(); ++program) {
    auto& pgm = state.original.programs.at(program);
    _programs.push_back(index);
    index += pgm.objects.size();
    for (auto object = 0; object < pgm.objects.size(); ++object) {
      auto widget = new ObjectParametersWidget(program, object, state);
      _stack->addWidget(widget);
    }
  }

  _layout->addWidget(form_widget);
  _layout->addWidget(_stack);

  connect(_objects, SIGNAL(activated(int)), this, SLOT(setObject(int)));
}

void ParametersWidget::setObject(int index) {
  index += _programs.at(_program);
  _stack->setCurrentIndex(index);
}

void ParametersWidget::handleProgramChange(ProgramChangeEvent event) {
  _program = event.program;
  _stack->setCurrentIndex(_programs.at(_program));
  _objects->setCurrentIndex(0);
}
