#include "hans/common/ObjectGraph.hpp"
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <vector>

using namespace hans::common;

static ObjectGraph::data* hans_graph_make(uint32_t objects_len,
                                          uint32_t connections_len,
                                          uint32_t arguments_len) {
  size_t s1 = sizeof(ObjectGraph::data);
  size_t s2 = objects_len * sizeof(hans_user_object);
  size_t s3 = connections_len * sizeof(hans_object_connection);
  size_t s4 = arguments_len * sizeof(hans_user_arg);

  char* buff = static_cast<char*>(std::calloc(1, s1 + s2 + s3 + s4));

  ObjectGraph::data* graph = reinterpret_cast<ObjectGraph::data*>(buff);
  graph->bytes = s1 + s2 + s3;
  graph->objects_len = objects_len;
  graph->connections_len = connections_len;
  graph->arguments_len = arguments_len;
  graph->objects = reinterpret_cast<hans_user_object*>(buff + s1);
  graph->connections =
      reinterpret_cast<hans_object_connection*>(buff + s1 + s2);
  graph->arguments = reinterpret_cast<hans_user_arg*>(buff + s1 + s2 + s3);
  return graph;
}

static void hans_graph_destroy(ObjectGraph::data* graph) {
  std::free(graph);
}

static bool hans_graph_topological_sort(ObjectGraph::data* graph) {
  if (graph->objects_len > 1) {
    // XXX: Check that no instance ids repeat?
    bool all_zero = true;
    for (int i = 0; i < graph->objects_len; ++i) {
      if (graph->objects[i].instance_id != 0) {
        all_zero = false;
      }
    }

    assert(!all_zero && "Expected instance ids to be set");
  }

  // Create a new graph, of the same size, to store the sorted data in
  auto sorted_nodes = hans_graph_make(
      graph->objects_len, graph->connections_len, graph->arguments_len);
  sorted_nodes->objects_len = 0;
  sorted_nodes->connections_len = 0;

  // Copy input graphs edges, so we can remove edges without modifying input
  while (sorted_nodes->connections_len < graph->connections_len) {
    auto num_edges = sorted_nodes->connections_len;
    sorted_nodes->connections[num_edges] = graph->connections[num_edges];
    sorted_nodes->connections_len++;
  }

  // Keep track of the objects previous position in the graph
  typedef struct {
    hans_user_object node;
    uint32_t index;
  } inter_node;

  auto source_nodes = std::make_unique<inter_node[]>(graph->objects_len);
  auto source_nodes_len = 0;

  // Find all source nodes (ones with no incoming edges)
  for (int i = 0; i < graph->objects_len; ++i) {
    bool incoming = false;

    for (int e = 0; e < graph->connections_len; ++e) {
      if (graph->connections[e].sink == i) {
        incoming = true;
      }
    }

    if (!incoming) {
      // Delete the edge by decrementing at the location to delete
      source_nodes[source_nodes_len].node = graph->objects[i];
      source_nodes[source_nodes_len].index = i;
      source_nodes_len++;
    }
  }

  while (source_nodes_len != 0) {
    // Pop a node off the end of 'source_nodes'
    source_nodes_len--;
    auto src_node = source_nodes[source_nodes_len];

    // Move sorted node into 'sorted_nodes'
    sorted_nodes->objects[sorted_nodes->objects_len] = src_node.node;
    sorted_nodes->objects_len++;

    // for each node 'M' with an edge 'E' from 'N' to 'M' do
    for (int n = 0; n < graph->objects_len; ++n) {
      auto node = graph->objects[n];
      auto len = sorted_nodes->connections_len;

      while (len--) {
        auto edge = sorted_nodes->connections[len];

        // If the edge does not contain a connect
        if (edge.source != src_node.index || edge.sink != n) {
          continue;
        }

        // Remove the edge from 'sorted_nodes' (copy of the original graph)
        auto shift = len + 1;

        // Delete the edge by decrementing at the location to delete
        while (shift < sorted_nodes->connections_len) {
          sorted_nodes->connections[shift - 1] =
              sorted_nodes->connections[shift];
          shift++;
        }

        // Decrement the number of edges
        sorted_nodes->connections_len--;

        // Check if 'M' has no other incoming edges
        bool incoming = false;

        for (int e = 0; e < sorted_nodes->connections_len; ++e) {
          if (sorted_nodes->connections[e].sink == n) {
            incoming = true;
            break;
          }
        }

        // If there were no incoming edges insert 'M' into 'source_nodes'
        if (!incoming) {
          source_nodes[source_nodes_len].node = node;
          source_nodes[source_nodes_len].index = n;
          source_nodes_len++;
        }
      }
    }
  }

  bool success = sorted_nodes->objects_len == graph->objects_len;
  // If the sort was not successful, then the graph contains a cycle etc.
  if (!success) {
    hans_graph_destroy(sorted_nodes);
    return false;
  }

  // Re-write the edge list of the graph, now that the node list if ordered
  // differently, the edge indexes will no longer match up
  for (int e = 0; e < graph->connections_len; ++e) {
    auto edge = graph->connections[e];
    auto src_node = graph->objects[edge.source];
    auto sink_node = graph->objects[edge.sink];

    int matched = 0;

    for (int n = 0; n < sorted_nodes->objects_len; ++n) {
      if (sorted_nodes->objects[n].instance_id == sink_node.instance_id) {
        graph->connections[e].sink = n;
        matched++;
      } else if (sorted_nodes->objects[n].instance_id == src_node.instance_id) {
        graph->connections[e].source = n;
        matched++;
      }

      if (matched == 2) {
        break;
      }
    }
  }

  // Finally copy the sorted nodes into the graph
  for (int i = 0; i < graph->objects_len; ++i) {
    graph->objects[i] = sorted_nodes->objects[i];
  }

  hans_graph_destroy(sorted_nodes);
  return true;
}

bool hans_graph_edges_sort(ObjectGraph::data* graph) {
  int size = graph->objects_len;

  std::vector<hans_instance_id> order;
  order.reserve(size);
  for (int n = 0; n < graph->objects_len; ++n) {
    order.push_back(graph->objects[n].instance_id);
  }

  auto compare = [&order, &size](auto& a, auto& b) {
    int a_order = -1;
    int b_order = -1;

    for (int n = 0; n < size; ++n) {
      if (order.at(n) == a.source) {
        a_order = n;
      }

      if (order.at(n) == b.source) {
        b_order = n;
      }

      if (a_order != -1 && b_order != -1) {
        break;
      }
    }

    return a_order < b_order;
  };

  std::sort(graph->connections, &graph->connections[graph->connections_len],
            compare);
  return true;
}

ObjectGraph::ObjectGraph(uint32_t num_objects, uint32_t num_connections,
                         uint32_t num_arguments) {
  m_data = hans_graph_make(num_objects, num_connections, num_arguments);
}

ObjectGraph::ObjectGraph() {
  m_data = nullptr;
}

ObjectGraph::ObjectGraph(ObjectGraph::data* data) : m_data(data) {
}

bool ObjectGraph::copy(const ObjectGraph& o) {
  clear();
  m_data = hans_graph_make(o.m_data->objects_len, o.m_data->connections_len,
                           o.m_data->arguments_len);
  std::memcpy(m_data, o.m_data, m_data->bytes);
  return true;
}

ObjectGraph::~ObjectGraph() {
  hans_graph_destroy(m_data);
}

void ObjectGraph::clear() {
  hans_graph_destroy(m_data);
  m_data = nullptr;
}

ObjectGraph::ObjectList ObjectGraph::get_objects() const {
  return ObjectGraph::ObjectList(m_data->objects, m_data->objects_len);
}

ObjectGraph::ConnectionList ObjectGraph::get_connections() const {
  return ObjectGraph::ConnectionList(m_data->connections,
                                     m_data->connections_len);
}

ObjectGraph::ArgumentList ObjectGraph::get_arguments() const {
  return ObjectGraph::ArgumentList(m_data->arguments, m_data->arguments_len);
}

hans_user_object* ObjectGraph::object_at(uint32_t index) const {
  return &m_data->objects[index];
}

hans_object_connection* ObjectGraph::connection_at(uint32_t index) const {
  return &m_data->connections[index];
}

hans_user_arg* ObjectGraph::argument_at(uint32_t index) const {
  return &m_data->arguments[index];
}

bool ObjectGraph::topological_sort() {
  return hans_graph_topological_sort(m_data);
}

void ObjectGraph::sort_edges() {
  hans_graph_edges_sort(m_data);
}
