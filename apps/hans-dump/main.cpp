#include <cstring>
#include <cxxopts.hpp>
#include <fstream>
#include <iostream>
#include <string>
#include "hans/common/DataLoader.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"

using namespace hans;
using namespace hans::common;

class Dumper {
 public:
  const StringManager& m_strings;

  explicit Dumper(const StringManager& strings) : m_strings(strings) {
  }

  void print(const char* label, const ListView<hans_library>& libraries) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values:" << std::endl;
    for (const auto& library : libraries) {
      std::cout << "  - Filepath: " << s.lookup(library.filepath) << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_object>& objects) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values:" << std::endl;
    for (const auto& object : objects) {
      std::cout << "  - ID: " << object.id << std::endl;

      switch (object.type) {
      case HANS_OBJECT_AUDIO:
        std::cout << "    Type: Audio" << std::endl;
        break;
      case HANS_OBJECT_GRAPHICS:
        std::cout << "    Type: Graphics" << std::endl;
        break;
      }

      std::cout << "    Name: " << s.lookup(object.name) << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_parameter>& parameters) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values:" << std::endl;
    for (const auto& parameter : parameters) {
      std::cout << "  - Object: " << parameter.object << std::endl;
      std::cout << "    Name: " << s.lookup(parameter.name) << std::endl;
      std::cout << "    Size: " << size_t(parameter.size) << std::endl;
      std::cout << "    Offset: " << parameter.offset << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_parameter_value>& values) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: ";
    for (const auto& value : values) {
      std::cout << value << " ";
    }
    std::cout << std::endl;
  }

  void print(const char* label, const ListView<hans_program>& programs) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& program : programs) {
      std::cout << "  - Name: " << s.lookup(program.name) << std::endl;
      std::cout << "    Audio: " << std::endl;
      std::cout << "    - ID: " << program.audio.id << std::endl;
      std::cout << "      start: " << program.audio.start << std::endl;
      std::cout << "      end: " << program.audio.end << std::endl;
      std::cout << "    Graphics: " << std::endl;
      std::cout << "    - ID: " << program.graphics.id << std::endl;
      std::cout << "      start: " << program.graphics.start << std::endl;
      std::cout << "      end: " << program.graphics.end << std::endl;
    }
  }

  void print(const char* label, const ListView<size_t>& values) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: ";
    for (const auto& value : values) {
      std::cout << value << " ";
    }
    std::cout << std::endl;
  }

  void print(const char* label, const ListView<hans_fbo>& fbos) {
    std::cout << "- Type: FBOS" << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& fbo : fbos) {
      std::cout << "  - Object: " << fbo.object << std::endl;
      std::cout << "    Stencil: " << fbo.stencil_buffer << std::endl;
      std::cout << "    Start: " << fbo.start << std::endl;
      std::cout << "    End: " << fbo.end << std::endl;
    }
  }

  void print(const char* label,
             const ListView<hans_fbo_attachment>& attachments) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& attachment : attachments) {
      switch (attachment.type) {
      case HANS_COLOR_ATTACHMENT:
        std::cout << "  - Type: Color" << std::endl;
        break;
      case HANS_DEPTH_ATTACHMENT:
        std::cout << "  - Type: Depth" << std::endl;
        break;
      case HANS_STENCIL_ATTACHMENT:
        std::cout << "  - Type: Stencil" << std::endl;
        break;
      }

      std::cout << "    Width: " << attachment.width << std::endl;
      std::cout << "    Height: " << attachment.height << std::endl;
      std::cout << "    Components: " << attachment.components << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_shader>& shaders) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& shader : shaders) {
      switch (shader.type) {
      case HANS_SHADER_VERTEX:
        std::cout << "  - Type: Vertex" << std::endl;
        break;
      case HANS_SHADER_FRAGMENT:
        std::cout << "  - Type: Fragment" << std::endl;
        break;
      }

      std::cout << "    Name: " << s.lookup(shader.name) << std::endl;
      std::cout << "    Code: " << shader.code << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_audio_buffer>& buffers) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& buffer : buffers) {
      std::cout << "  - Object: " << buffer.object << std::endl;
      std::cout << "    Name: " << s.lookup(buffer.name) << std::endl;
      std::cout << "    Channels: " << size_t(buffer.channels) << std::endl;
      std::cout << "    Size: " << buffer.size << std::endl;
    }
  }

  void print(const char* label, const ListView<hans_register>& registers) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& reg : registers) {
      std::cout << "  - Object: " << reg.object << std::endl;

      switch (reg.type) {
      case HANS_OBJECT_GRAPHICS:
        std::cout << "    Type: Graphics" << std::endl;
        break;
      case HANS_OBJECT_AUDIO:
        std::cout << "    Type: Audio" << std::endl;
        break;
      }

      std::cout << "    Graph: " << reg.graph << std::endl;
      std::cout << "    Index: " << size_t(reg.index) << std::endl;
      std::cout << "    Bin: " << reg.bin << std::endl;
      if (reg.readonly) {
        std::cout << "    Readonly: True" << std::endl;
      } else {
        std::cout << "    Readonly: False" << std::endl;
      }
    }
  }

  void print(const char* label, const ListView<hans_hash>& hashes) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& hash : hashes) {
      std::cout << "  - Hash: " << hash << std::endl;
    }
  }

  void print(const char* label, const ListView<const char>& data) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    std::cout << std::string(&data[0], data.size()) << std::endl;
  }

  void print(const char* label,
             const ListView<hans_resource_request>& requests) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& req : requests) {
      switch (req.type) {
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

      std::cout << "    Amount: " << req.amount << std::endl;
    }
  }

  void print(const char* label, const hans_blob& blob) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Size: " << blob.size << std::endl;
    std::cout << "  Offset: " << blob.offset << std::endl;
  }
};

int main(int argc, char* argv[]) {
  cxxopts::Options options(argv[0], " <filename>");
  bool verbose = false;
  // clang-format off
  options.add_options()
    ("h,help", "Show this screen")
    ("verbose", "Verbose mode", cxxopts::value<bool>(verbose));

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

  auto reader = DataReader(args.at(0).c_str());
  auto strings = StringManager(reader.data.string_hashes,
                               reader.data.string_offsets, reader.data.strings);
  auto printer = Dumper(strings);
  printer.print("Libraries", reader.data.libraries);
  printer.print("Objects", reader.data.objects);
  printer.print("Programs", reader.data.programs);
  printer.print("Chains", reader.data.chains);
  printer.print("Registers", reader.data.registers);
  printer.print("Parameters", reader.data.parameters);
  printer.print("Parameter values", reader.data.parameter_values);
  printer.print("FBOs", reader.data.fbos);
  printer.print("FBO Attachments", reader.data.fbo_attachments);
  printer.print("Shaders", reader.data.shaders);
  printer.print("Audio buffers", reader.data.audio_buffers);

  if (verbose) {
    printer.print("String hashes", reader.data.string_hashes);
    printer.print("String offsets", reader.data.string_offsets);
    printer.print("String values", reader.data.strings);
  }

  return 0;
};
