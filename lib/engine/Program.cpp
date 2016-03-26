#include "hans/engine/Program.hpp"
#include <iterator>
#include <sstream>
#include <memory>

using namespace hans;

class ObjectConstructorApi {
 public:
  ObjectConstructorApi();
  void assign_id(hans_user_object* objects);
  void set_object(hans_user_object* object);

  hans_object_arguments get_args();
  void request_resource(hans_resource_type type, int amount);
  int get_num_resources(hans_resource_type type) const;
  int get_num_resources(hans_resource_type type, hans_instance_id id) const;
  int get_total_resources() const;

 private:
  typedef struct {
    hans_instance_id id;
    hans_resource_type type;
    int count;
  } resource_count;
  std::vector<resource_count> m_count;
  hans_user_object* m_object;
  uint32_t m_instance_id;
};

ObjectConstructorApi::ObjectConstructorApi() {
  m_instance_id = 0;
}

void ObjectConstructorApi::assign_id(hans_user_object* object) {
  object->instance_id = m_instance_id;
  m_instance_id++;
}

void ObjectConstructorApi::set_object(hans_user_object* object) {
  m_object = object;
}

hans_object_arguments ObjectConstructorApi::get_args() {
  return {.length = m_object->arguments_len, .data = m_object->arguments};
}

void ObjectConstructorApi::request_resource(hans_resource_type type, int num) {
  m_count.push_back({.id = m_object->instance_id, .type = type, .count = num});
}

int ObjectConstructorApi::get_num_resources(hans_resource_type type) const {
  int count = 0;
  for (auto& seen : m_count) {
    if (seen.type == type) {
      count += seen.count;
    }
  }
  return count;
}

int ObjectConstructorApi::get_num_resources(hans_resource_type type,
                                            hans_instance_id id) const {
  int count = 0;
  for (auto& seen : m_count) {
    if (seen.type == type && seen.id == id) {
      count += seen.count;
    }
  }
  return count;
}

int ObjectConstructorApi::get_total_resources() const {
  int count = 0;
  for (auto& seen : m_count) {
    count += seen.count;
  }
  return count;
}

static void request_resource(hans_constructor_api* api, hans_resource_type type,
                             int amount) {
  static_cast<ObjectConstructorApi*>(api->data)->request_resource(type, amount);
}

static hans_object_arguments get_args(hans_constructor_api* api) {
  return static_cast<ObjectConstructorApi*>(api->data)->get_args();
}

template <typename T>
static bool create_chain(common::ObjectGraph& graph,
                         engine::ObjectChain<T>& chain,
                         hans_constructor_api* api,
                         const std::vector<hans_object>& valid_objects,
                         common::Logger* logger) {
  size_t chain_size = 0;
  std::vector<hans_object_id> not_found;
  auto object_list = graph.get_objects();

  for (const auto& object : object_list) {
    auto id = object.object_id;
    auto end = valid_objects.end();
    auto begin = valid_objects.begin();
    auto it = std::find_if(begin, end, [id](const hans_object& object) {
      return object.id == id;
    });

    if (it == end) {
      not_found.push_back(id);
      continue;
    }

    chain_size += (*it).size;
  }

  if (not_found.size() > 0) {
    std::ostringstream ss;
    ss << "PROGRAM Unknown object(s) found: ";
    std::copy(not_found.begin(), not_found.end() - 1,
              std::ostream_iterator<int>(ss, ", "));
    ss << not_found.back();
    logger->log(common::Logger::ERROR, ss.str().c_str());
    return false;
  }

  auto object_constructor = static_cast<ObjectConstructorApi*>(api->data);
  for (auto& object : object_list) {
    object_constructor->assign_id(&object);
  }

  if (!graph.topological_sort()) {
    logger->log(common::Logger::ERROR, "PROGRAM Unable to sort graph");
    return false;
  }

  if (!chain.set_capacity(object_list.size(), chain_size)) {
    logger->log(common::Logger::ERROR, "PROGRAM Not enough memory");
    return false;
  }

  for (auto& object : object_list) {
    object_constructor->set_object(&object);
    if (!chain.create_object(api, object.object_id, object.instance_id)) {
      std::ostringstream ss;
      ss << "PROGRAM Unable to create object: " << object.object_id;
      logger->log(common::Logger::ERROR, ss.str().c_str());
      return false;
    }
  }

  return true;
}

void log_resource(common::Logger* logger, hans_object_id id, const char* name,
                  uint32_t num) {
  std::ostringstream ss;
  ss << "PROGRAM Created resource object_id=" << id << " name=";
  ss << name << " number=" << num;
  logger->log(common::Logger::DEBUG, ss.str().c_str());
}

template <typename T>
int create_resources(hans_object_resource* res, int idx,
                     engine::ObjectChain<T>& chain,
                     const ObjectConstructorApi& const_api,
                     hans_object_api& obj_api) {
  int object_index = 0;
  for (T& object : chain) {
    auto previous = idx;
    object.resources = &res[idx];

    auto object_id = object.object_id;
    auto instance = object.instance_id;

    auto inlets = const_api.get_num_resources(HANS_INLET, instance);
    auto outlets = const_api.get_num_resources(HANS_OUTLET, instance);
    auto shaders = const_api.get_num_resources(HANS_SHADER, instance);
    auto au_buffers = const_api.get_num_resources(HANS_AUDIO_BUFFER, instance);

    idx += obj_api.parameters->make(&res[idx], object_id, instance);
    idx += obj_api.registers->make(&res[idx], object_index, inlets, outlets);

    // XXX: Maybe just assign audio resources separate to graphics resources
    if (obj_api.shaders != nullptr) {
      idx += obj_api.shaders->make(&res[idx], shaders);
      idx += obj_api.frame_buffers->make(&res[idx], object_id, instance);
    } else {
      idx += obj_api.audio_buffers->make(&res[idx], au_buffers);
    }

    object_index++;

    object.num_resources = idx - previous;
    if (object.num_resources == 0) {
      object.resources = nullptr;
    }
  }

  return idx;
}

template <typename T>
static bool setup(engine::ObjectChain<T>& chain, hans_object_api& object_api) {
  for (T& object : chain) {
    if (object.setup != nullptr) {
      object.setup(&object, &object_api);
    }
  }

  return true;
}

engine::Program::Program(engine::ProgramResources& resources)
    : m_resources(resources),
      m_parameter_manager(*resources.parameters),
      m_audio_register_manager(sizeof(hans_audio_buffer)),
      m_graphics_register_manager(sizeof(uint32_t)),
      m_shader_manager(*resources.logger, *resources.strings,
                       *resources.shaders),
      m_frame_buffer_manager(*resources.frame_buffers),
      m_audio_chain(*resources.audio_objects),
      m_graphics_chain(*resources.graphics_objects) {
  m_audio_api.config = m_resources.config;
  m_audio_api.logger = m_resources.logger;
  m_audio_api.strings = m_resources.strings;
  m_audio_api.parameters = &m_parameter_manager;
  m_audio_api.audio_buffers = m_resources.audio_buffers;
  m_audio_api.audio_buses = m_resources.audio_buses;
  m_audio_api.shaders = nullptr;
  m_graphics_api.frame_buffers = nullptr;
  m_audio_api.registers = &m_audio_register_manager;

  m_graphics_api.config = m_resources.config;
  m_graphics_api.logger = m_resources.logger;
  m_graphics_api.strings = m_resources.strings;
  m_graphics_api.parameters = &m_parameter_manager;
  m_graphics_api.audio_buffers = nullptr;
  m_graphics_api.shaders = &m_shader_manager;
  m_graphics_api.frame_buffers = &m_frame_buffer_manager;
  m_graphics_api.registers = &m_graphics_register_manager;
}

void engine::Program::process_audio() {
  for (auto& object : m_audio_chain) {
    object.callback(&object, &m_audio_api);
  }
}

void engine::Program::process_graphics() {
  for (auto& object : m_graphics_chain) {
    if (object.update != nullptr) {
      object.update(&object, &m_graphics_api);
    }
  }

  for (auto& object : m_graphics_chain) {
    if (object.draw != nullptr) {
      object.draw(&object, &m_graphics_api);
    }
  }
}

void engine::Program::destroy() {
  m_graphics_chain.destroy();
  m_audio_chain.destroy();
}

bool engine::Program::set(common::ObjectGraph& audio_graph,
                          common::ObjectGraph& graphics_graph) {
  ObjectConstructorApi object_constructor;
  hans_constructor_api constructor_api;
  constructor_api.get_args = get_args;
  constructor_api.request_resource = request_resource;
  constructor_api.data = &object_constructor;

  // Calls all the requested objects constructors
  bool success = create_chain<hans_audio_object>(
      audio_graph, m_audio_chain, &constructor_api, *m_resources.audio_objects,
      m_resources.logger);

  if (!success) {
    return false;
  }

  success = create_chain<hans_graphics_object>(
      graphics_graph, m_graphics_chain, &constructor_api,
      *m_resources.graphics_objects, m_resources.logger);

  if (!success) {
    return false;
  }

  // Report data back to the user graph
  for (auto& o : audio_graph.get_objects()) {
    o.inlets = object_constructor.get_num_resources(HANS_INLET, o.instance_id);
    o.outlets =
        object_constructor.get_num_resources(HANS_OUTLET, o.instance_id);
  }

  for (auto& o : graphics_graph.get_objects()) {
    o.inlets = object_constructor.get_num_resources(HANS_INLET, o.instance_id);
    o.outlets =
        object_constructor.get_num_resources(HANS_OUTLET, o.instance_id);
  }

  // Log the data now that we have, at the very least, created the object chains
  m_resources.logger->log(common::Logger::DEBUG, audio_graph);
  m_resources.logger->log(common::Logger::DEBUG, graphics_graph);

  // Update managers now that we have more information on the number of
  // resources needed to execute the graph
  auto gfx_connections = graphics_graph.get_connections();
  m_graphics_api.registers->set_interference_graph(gfx_connections.get(),
                                                   gfx_connections.size());
  auto au_connections = audio_graph.get_connections();
  m_audio_api.registers->set_interference_graph(au_connections.get(),
                                                au_connections.size());

  // A single list of all the object ID's used in both graphs
  std::vector<hans_object_id> object_ids;
  object_ids.reserve(audio_graph.get_objects().size() +
                     graphics_graph.get_objects().size());
  for (auto& object : audio_graph.get_objects()) {
    object_ids.push_back(object.object_id);
  }
  for (auto& object : graphics_graph.get_objects()) {
    object_ids.push_back(object.object_id);
  }

  auto num_param_handles = m_parameter_manager.set_objects(object_ids);
  auto num_frame_buffers = m_frame_buffer_manager.set_objects(object_ids);

  // Create all the resource handles for the objects
  int resources = (object_constructor.get_total_resources() +
                   num_param_handles + num_frame_buffers);
  auto resource_list = std::make_unique<hans_object_resource[]>(resources);

  int i = 0;
  i = create_resources(resource_list.get(), i, m_audio_chain,
                       object_constructor, m_audio_api);
  i = create_resources(resource_list.get(), i, m_graphics_chain,
                       object_constructor, m_graphics_api);

  if (i > resources) {
    std::ostringstream ss;
    ss << "PROGRAM Created to many resources expected=";
    ss << resources << " created=" << i;
    m_resources.logger->log(common::Logger::ERROR, ss.str().c_str());
    return false;
  }

  // Call the objects setup functions, allowing the chains to be executed
  if (!setup<hans_audio_object>(m_audio_chain, m_audio_api)) {
    return false;
  }

  if (!setup<hans_graphics_object>(m_graphics_chain, m_graphics_api)) {
    return false;
  }

  return true;
}
