#include <libguile.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/hasher.hpp"
#include "hans/common/procedure.hpp"
#include "hans/common/smobs.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/PluginManager.hpp"
#include "hans/engine/ShaderManager.hpp"
#include "hans/engine/gl.hpp"
#include "hans/engine/object.hpp"

using namespace hans;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

static SCM validate_shaders(SCM engine_data) {
  auto res = glfwInit();
  if (res != GL_TRUE) {
    return SCM_BOOL_F;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
  glfwWindowHint(GLFW_FOCUSED, GL_FALSE);
  glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
  glfwWindowHint(GLFW_DECORATED, GL_FALSE);

  auto window = glfwCreateWindow(480, 320, "Shaders", nullptr, nullptr);
  glfwMakeContextCurrent(window);

  auto out = SCM_EOL;

  {
    auto ng = scm::to_cpp<EngineData>(engine_data);
    StringManager strings(ng.strings);
    ShaderManager shader_manager(strings, ng.shaders);

    for (const auto& shader : ng.shaders) {
      auto message = shader_manager.validate(shader.name);

      auto out_valid = SCM_BOOL_T;
      auto out_message = SCM_BOOL_F;

      if (message != nullptr) {
        out_valid = SCM_BOOL_F;
        out_message = scm_from_locale_string(message);
        delete[] message;
      }

      out = scm_cons(scm_cons(out_valid, out_message), out);
    }
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return scm_reverse(out);
}

class GuilePatcher : public IPatcher {
 public:
  GuilePatcher(StringManager& strings, const std::vector<ObjectDef>& objects,
               Arguments& arguments)
      : m_strings(strings), m_objects(objects), m_arguments(arguments) {
    m_current = 0;
    m_error = false;
  }

  bool object_error() {
    return m_error;
  }

  virtual std::vector<Argument> arguments() {
    auto offset = m_arguments.offsets.at(m_current);
    auto length = m_arguments.lengths.at(m_current);
    return std::vector<Argument>(&m_arguments.arguments[offset],
                                 &m_arguments.arguments[offset + length]);
  }

  virtual void invalid(const char* name) {
    auto object = m_strings.lookup(m_objects.at(m_current).name);
    std::cout << "[HANS] (" << m_current << ") " << object
              << " Invalid value for argument '" << name << "'" << std::endl;
    m_error = true;
  }

  virtual void missing(const char* name) {
    auto object = m_strings.lookup(m_objects.at(m_current).name);
    std::cout << "[HANS] (" << m_current << ") " << object
              << " Missing required argument '" << name << "'" << std::endl;
    m_error = true;
  }

  virtual void request(IPatcher::Resources type, size_t value) {
    auto& smobs = scm::detail::Smobs::get();
    auto scm_type = smobs.enum_to_scm("RESOURCES", type);

    switch (type) {
    case IPatcher::Resources::INLET:
    case IPatcher::Resources::OUTLET: {
      m_requests.push_back(scm_cons(scm_type, scm_from_int(value)));
      break;
    }
    case IPatcher::Resources::RING_BUFFER: {
      auto name = m_strings.lookup(value);
      auto str = scm_from_locale_string(name);
      m_requests.push_back(scm_cons(scm_type, str));
      break;
    }
    default:
      throw std::runtime_error("Unkown resource request");
    }
  }

  std::vector<SCM> get_requests() {
    return m_requests;
  }

  void advance() {
    m_requests.clear();
    m_current += 1;
    m_error = false;
  }

 private:
  bool m_error;
  StringManager& m_strings;
  const std::vector<ObjectDef>& m_objects;
  Arguments& m_arguments;
  std::vector<SCM> m_requests;
  size_t m_current;
};

static SCM configure_objects(SCM scm_engine_data, SCM scm_arguments) {
  auto args = scm::to_cpp<Arguments>(scm_arguments);
  auto ng = scm::to_cpp<EngineData>(scm_engine_data);
  auto output = SCM_EOL;

  {
    StringManager strings(ng.strings);
    PluginManager plugins(strings, ng.objects, ng.plugins);
    GuilePatcher patcher(strings, ng.objects, args);

    for (const auto& factory : ng.objects) {
      std::cout << strings.lookup(factory.name) << std::endl;
      std::cout << "===" << std::endl;
      auto object = plugins.create(factory.name);

      auto resources = SCM_EOL;
      auto state = SCM_EOL;

      if (object != nullptr) {
        object->create(patcher);

        if (!patcher.object_error()) {
          auto data = plugins.serialize(factory.name, object);
          state = scm_from_locale_string(data.c_str());
          for (auto& request : patcher.get_requests()) {
            resources = scm_cons(request, resources);
          }
        }
      }

      std::cout << "==destroystart==" << std::endl;
      plugins.destroy(factory.name, object);
      std::cout << "==destroyend==" << std::endl;
      patcher.advance();
      output = scm_cons(scm_cons(state, resources), output);
    }
    std::cout << "Exiting scope" << std::endl;
  }

  return scm_reverse(output);
}

extern "C" {
void scm_init_hans_compiler_module() {
  scm::procedure<validate_shaders>("%validate-shaders", 1, 0, 0);
  scm::procedure<configure_objects>("%configure-objects", 2, 0, 0);
}
}
