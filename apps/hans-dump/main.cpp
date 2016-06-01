#include <cstring>
#include <cxxopts.hpp>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "hans/common/DataLoader.hpp"
#include "hans/common/types.hpp"

using namespace hans;

static void dump_libraries(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_library);

  while (ptr != end) {
    auto library = reinterpret_cast<hans_library*>(ptr);
    std::cout << "  - Filepath: " << library->filepath << std::endl;
    ptr += step;
  }
}

static void dump_objects(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_object);

  while (ptr != end) {
    auto object = reinterpret_cast<hans_object*>(ptr);
    std::cout << "  - ID: " << object->id << std::endl;

    switch (object->type) {
    case HANS_AUDIO:
      std::cout << "    Type: Audio" << std::endl;
      break;
    case HANS_GRAPHICS:
      std::cout << "    Type: Graphics" << std::endl;
      break;
    }

    std::cout << "    Name: " << object->name << std::endl;
    std::cout << "    Size: " << object->size << std::endl;
    ptr += step;
  }
}

static void dump_parameters(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_parameter);

  while (ptr != end) {
    auto parameter = reinterpret_cast<hans_parameter*>(ptr);
    std::cout << "  - ID: " << parameter->id << std::endl;
    std::cout << "    Name: " << parameter->name << std::endl;
    std::cout << "    Size: " << parameter->size << std::endl;
    ptr += step;
  }
}

static void dump_parameter_values(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_parameter_value);

  std::cout << "  - Value: ";

  while (ptr != end) {
    auto value = *reinterpret_cast<hans_parameter_value*>(ptr);
    std::cout << value << " ";
    ptr += step;
  }

  std::cout << std::endl;
}

static void dump_graphs(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(size_t);

  std::cout << "  - Value: ";

  while (ptr != end) {
    auto value = *reinterpret_cast<size_t*>(ptr);
    std::cout << value << " ";
    ptr += step;
  }

  std::cout << std::endl;
}

static void dump_connections(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_object_connection);

  while (ptr != end) {
    auto conn = reinterpret_cast<hans_object_connection*>(ptr);
    std::cout << "  - Source: " << conn->source << std::endl;
    std::cout << "    Outlet: " << conn->outlet << std::endl;
    std::cout << "    Sink: " << conn->sink << std::endl;
    std::cout << "    Inlet: " << conn->inlet << std::endl;
    ptr += step;
  }
}

static void dump_resource_requests(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_resource_request);

  while (ptr != end) {
    auto request = reinterpret_cast<hans_resource_request*>(ptr);

    switch (request->type) {
    case HANS_PARAMETER:
      std::cout << "  - Type: HANS_PARAMETER" << std::endl;
      break;
    case HANS_SHADER:
      std::cout << "  - Type: HANS_SHADER" << std::endl;
      break;
    case HANS_AUDIO_BUFFER:
      std::cout << "  - Type: HANS_AUDIO_BUFFER" << std::endl;
      break;
    case HANS_FRAME_BUFFER:
      std::cout << "  - Type: HANS_FRAME_BUFFER" << std::endl;
      break;
    case HANS_INLET:
      std::cout << "  - Type: HANS_INLET" << std::endl;
      break;
    case HANS_OUTLET:
      std::cout << "  - Type: HANS_OUTLET" << std::endl;
      break;
    }

    std::cout << "    Amount: " << request->amount << std::endl;
    ptr += step;
  }
}

static void dump_programs(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_program);

  while (ptr != end) {
    auto program = reinterpret_cast<hans_program*>(ptr);
    std::cout << "  - Name: " << program->name << std::endl;
    std::cout << "    Audio node start: " << program->audio.node_start
              << std::endl;
    std::cout << "    Audio node end: " << program->audio.node_end << std::endl;

    std::cout << "    Audio edge start: " << program->audio.edge_start
              << std::endl;
    std::cout << "    Audio edge end: " << program->audio.edge_end << std::endl;

    std::cout << "    Graphics node start: " << program->graphics.node_start
              << std::endl;
    std::cout << "    Graphics node end: " << program->graphics.node_end
              << std::endl;

    std::cout << "    Graphics edge start: " << program->graphics.edge_start
              << std::endl;
    std::cout << "    Graphics edge end: " << program->graphics.edge_end
              << std::endl;
    ptr += step;
  }
}

static void dump_fbos(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_fbo);

  while (ptr != end) {
    auto fbo = reinterpret_cast<hans_fbo*>(ptr);
    std::cout << "  - Object ID: " << fbo->object_id << std::endl;
    std::cout << "    Stencil: " << fbo->stencil_buffer << std::endl;
    std::cout << "    Start: " << fbo->start << std::endl;
    std::cout << "    End: " << fbo->end << std::endl;
    ptr += step;
  }
}

static void dump_fbo_attachments(const hans_blob& blob) {
  auto ptr = static_cast<char*>(blob.data);
  auto end = ptr + blob.size;
  auto step = sizeof(hans_fbo_attachment);

  while (ptr != end) {
    auto attachment = reinterpret_cast<hans_fbo_attachment*>(ptr);
    switch (attachment->type) {
    case HANS_COLOR_ATTACHMENT:
      std::cout << "  - Type: HANS_COLOR_ATTACHMENT" << std::endl;
      break;
    case HANS_DEPTH_ATTACHMENT:
      std::cout << "  - Type: HANS_DEPTH_ATTACHMENT" << std::endl;
      break;
    case HANS_STENCIL_ATTACHMENT:
      std::cout << "  - Type: HANS_STENCIL_ATTACHMENT" << std::endl;
      break;
    }

    std::cout << "    Width: " << attachment->width << std::endl;
    std::cout << "    Height: " << attachment->height << std::endl;
    std::cout << "    Components: " << attachment->components << std::endl;
    ptr += step;
  }
}

int main(int argc, char* argv[]) {
  cxxopts::Options options(argv[0], " <filename>");

  // clang-format off
  options.add_options()
    ("h,help", "Show this screen");

  options.add_options("Hidden")
    ("positional", "Positional arguments (internal)",
     cxxopts::value<std::vector<std::string>>());
  // clang-format on

  options.parse_positional("positional");

  try {
    options.parse(argc, argv);
  } catch (const cxxopts::OptionException& e) {
    std::cout << "error parsing options: " << e.what() << std::endl;
    return 1;
  }

  auto args = options["positional"].as<std::vector<std::string>>();
  if (options.count("help") || args.size() != 1) {
    std::cout << options.help({""}) << std::endl;
    return 0;
  }

  common::DataReader reader(args.at(0).c_str());
  auto file = reader.file;
  std::cout << "Path: " << args.at(0) << std::endl;
  std::cout << "Length: " << file.length << std::endl;
  std::cout << "Bytes: " << file.size << std::endl;
  std::cout << "Blobs: " << std::endl;

  hans_blob string_values;
  hans_blob string_offsets;
  hans_blob string_hashes;

  for (auto i = 0; i < file.length; ++i) {
    auto blob = file.blobs[i];

    switch (blob.type) {
    case HANS_BLOB_STRINGS:
      std::cout << "- Type: HANS_BLOB_STRINGS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      string_values = blob;
      break;

    case HANS_BLOB_STRING_HASHES:
      std::cout << "- Type: HANS_BLOB_STRING_HASHES" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      string_hashes = blob;
      break;

    case HANS_BLOB_STRING_OFFSETS:
      std::cout << "- Type: HANS_BLOB_STRING_OFFSETS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      string_offsets = blob;
      break;

    case HANS_BLOB_LIBRARIES:
      std::cout << "- Type: HANS_BLOB_LIBRARIES" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_libraries(blob);
      break;

    case HANS_BLOB_OBJECTS:
      std::cout << "- Type: HANS_BLOB_OBJECTS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_objects(blob);
      break;

    case HANS_BLOB_OBJECTS_DATA:
      std::cout << "- Type: HANS_BLOB_OBJECTS_DATA" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      break;

    case HANS_BLOB_PARAMETERS:
      std::cout << "- Type: HANS_BLOB_PARAMETERS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_parameters(blob);
      break;

    case HANS_BLOB_PARAMETER_VALUES:
      std::cout << "- Type: HANS_BLOB_PARAMETER_VALUES" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_parameter_values(blob);
      break;

    case HANS_BLOB_SHADERS:
      std::cout << "- Type: HANS_BLOB_SHADERS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      break;

    case HANS_BLOB_FBOS:
      std::cout << "- Type: HANS_BLOB_FBOS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_fbos(blob);
      break;

    case HANS_BLOB_FBO_ATTACHMENTS:
      std::cout << "- Type: HANS_BLOB_FBO_ATTACHMENTS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_fbo_attachments(blob);
      break;

    case HANS_BLOB_PROGRAMS:
      std::cout << "- Type: HANS_BLOB_PROGRAM" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_programs(blob);
      break;

    case HANS_BLOB_PROGRAM_GRAPHS:
      std::cout << "- Type: HANS_BLOB_PROGRAM_GRAPHS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_graphs(blob);
      break;

    case HANS_BLOB_GRAPH_CONNECTIONS:
      std::cout << "- Type: HANS_BLOB_GRAPH_CONNECTIONS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_connections(blob);
      break;

    case HANS_BLOB_RESOURCE_REQUESTS:
      std::cout << "- Type: HANS_BLOB_RESOURCE_REQUESTS" << std::endl;
      std::cout << "  Size: " << blob.size << std::endl;
      std::cout << "  Data: " << std::endl;
      dump_resource_requests(blob);
      break;

    default:
      throw std::runtime_error("Unknown type");
    }
  }

  return 0;
};
