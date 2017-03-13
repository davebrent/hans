#include "state.hpp"
#include <QDebug>
#include <hans/engine.hpp>
#include <hans/hasher.hpp>
#include <hans/user_config_compiler.hpp>
#include <hans/user_config_loader.hpp>

using namespace gui;

State::State(const char* filepath) : _engine(nullptr) {
  if (hans::load_config(filepath, original)) {
    hans::compile_config(original, compiled);
  }

  for (const auto& program : original.programs) {
    _program_list.append(QString::fromStdString(program.name));
  }
  _programs.setStringList(_program_list);

  _timer = new QTimer(this);
  connect(_timer, SIGNAL(timeout()), this, SLOT(nextFrame()));
  _timer->start(1000.0 / 60.0);
}

size_t State::active_program() {
  return _program;
}

QStringListModel* State::programs() {
  return &_programs;
}

QStringListModel* State::objects() {
  return &_objects;
}

std::vector<hans::user_parameter> State::parameters(size_t program,
                                                    size_t object) {
  auto& pgm = original.programs.at(program);
  auto& obj = pgm.objects.at(object);
  auto& type = obj.type;
  for (const auto& plugin : original.plugins) {
    for (const auto& deff : plugin.objects) {
      if (deff.name.compare(type) == 0) {
        return deff.parameters;
      }
    }
  }
  return {};
}

void State::setEngine(hans::Engine* engine) {
  _engine = engine;
}

void State::nextFrame() {
  FrameChangeEvent event;
  emit frameChange(event);
}

void State::setProgram(size_t program) {
  _program = program;
  _objects_list.clear();
  auto& pgm = original.programs.at(program);
  for (const auto& object : pgm.objects) {
    _objects_list.append(QString::fromStdString(object.name));
  }
  _objects.setStringList(_objects_list);

  if (_engine) {
    _engine->set_program(_program);
  }

  ProgramChangeEvent event;
  event.program = program;
  emit programChange(event);
}

void State::setProgram(int program) {
  setProgram((size_t)program);
}

void State::setParameter(const hans::Track track, float value, bool state) {
  if (_engine && state) {
    auto object = track.object;
    _engine->set_parameter(object, track.parameter, track.component, value);
  }

  ParameterChangeEvent event;
  emit parameterChange(event);
}

void State::setParameter(ParameterChangeEvent event) {
  auto& obj = original.programs.at(event.program).objects.at(event.object);
  auto name = hans::hasher(obj.name.c_str());
  for (const auto& object : compiled.programs.graphics.objects) {
    if (object.variable == name) {
      _engine->set_parameter(object.id, event.name, event.component,
                             event.value);
      emit parameterChange(event);
      return;
    }
  }
}
