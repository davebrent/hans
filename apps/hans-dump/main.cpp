#include <cstring>
#include <cxxopts.hpp>
#include <fstream>
#include <iostream>
#include <string>
#include "hans/common/DataLoader.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;
using namespace hans::graphics;

class Dumper {
 public:
  const StringManager& m_strings;

  explicit Dumper(const StringManager& strings) : m_strings(strings) {
  }

  void print(const char* label, const ListView<Library>& libraries) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values:" << std::endl;
    for (const auto& library : libraries) {
      std::cout << "  - Filepath: " << s.lookup(library.filepath) << std::endl;
    }
  }

  void print(const char* label, const ListView<ObjectDef>& objects) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values:" << std::endl;
    for (const auto& object : objects) {
      std::cout << "  - ID: " << object.id << std::endl;

      switch (object.type) {
      case ObjectDef::Types::AUDIO:
        std::cout << "    Type: Audio" << std::endl;
        break;
      case ObjectDef::Types::GRAPHICS:
        std::cout << "    Type: Graphics" << std::endl;
        break;
      }

      std::cout << "    Name: " << s.lookup(object.name) << std::endl;
    }
  }

  void print(const char* label, const ListView<Parameter>& parameters) {
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

  void print(const char* label, const ListView<Parameter::Value>& values) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: ";
    for (const auto& value : values) {
      std::cout << value << " ";
    }
    std::cout << std::endl;
  }

  void print(const char* label, const ListView<Program>& programs) {
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

  // void print(const char* label, const ListView<size_t>& values) {
  //   std::cout << "- Type: " << label << std::endl;
  //   std::cout << "  Values: ";
  //   for (const auto& value : values) {
  //     std::cout << value << " ";
  //   }
  //   std::cout << std::endl;
  // }

  void print(const char* label, const ListView<graphics::FBO>& fbos) {
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
             const ListView<graphics::FBO::Attachment>& attachments) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& attachment : attachments) {
      switch (attachment.type) {
      case graphics::FBO::Attachment::Types::COLOR:
        std::cout << "  - Type: Color" << std::endl;
        break;
      case graphics::FBO::Attachment::Types::DEPTH:
        std::cout << "  - Type: Depth" << std::endl;
        break;
      case graphics::FBO::Attachment::Types::STENCIL:
        std::cout << "  - Type: Stencil" << std::endl;
        break;
      }

      std::cout << "    Width: " << attachment.width << std::endl;
      std::cout << "    Height: " << attachment.height << std::endl;
      std::cout << "    Components: " << attachment.components << std::endl;
    }
  }

  void print(const char* label, const ListView<graphics::Shader>& shaders) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& shader : shaders) {
      switch (shader.type) {
      case graphics::Shader::Types::VERTEX:
        std::cout << "  - Type: Vertex" << std::endl;
        break;
      case graphics::Shader::Types::FRAGMENT:
        std::cout << "  - Type: Fragment" << std::endl;
        break;
      }

      std::cout << "    Name: " << s.lookup(shader.name) << std::endl;
      std::cout << "    Code: " << shader.code << std::endl;
    }
  }

  void print(const char* label, const ListView<audio::Buffer>& buffers) {
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

  void print(const char* label, const ListView<RingBuffer>& buffers) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& buffer : buffers) {
      std::cout << "  - Producer: " << buffer.producer << std::endl;
      std::cout << "    Name: " << s.lookup(buffer.name) << std::endl;
    }
  }

  void print(const char* label, const ListView<Modulator>& modulators) {
    const auto& s = m_strings;
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& mod : modulators) {
      std::cout << "  - Offset: " << mod.offset << std::endl;
      std::cout << "    Scale: " << mod.scale << std::endl;
      std::cout << "    Source: " << std::endl;
      std::cout << "    - Object: " << mod.source.object << std::endl;
      std::cout << "      Parameter: " << s.lookup(mod.source.parameter)
                << std::endl;
      std::cout << "      Component: " << size_t(mod.source.component)
                << std::endl;
      std::cout << "    Destination: " << std::endl;
      std::cout << "    - Object: " << mod.dest.object << std::endl;
      std::cout << "      Parameter: " << s.lookup(mod.dest.parameter)
                << std::endl;
      std::cout << "      Component: " << size_t(mod.dest.component)
                << std::endl;
    }
  }

  void print(const char* label, const ListView<Register>& registers) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& reg : registers) {
      std::cout << "  - Object: " << reg.object << std::endl;

      switch (reg.type) {
      case ObjectDef::Types::GRAPHICS:
        std::cout << "    Type: Graphics" << std::endl;
        break;
      case ObjectDef::Types::AUDIO:
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

  void print(const char* label, const ListView<hash>& hashes) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    for (const auto& hashed : hashes) {
      std::cout << "  - Value: " << hashed << std::endl;
    }
  }

  void print(const char* label, const ListView<const char>& data) {
    std::cout << "- Type: " << label << std::endl;
    std::cout << "  Values: " << std::endl;
    std::cout << std::string(&data[0], data.size()) << std::endl;
  }

  void print(const char* label, const DataFile::Blob& blob) {
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
  // printer.print("Chains", reader.data.chains);
  printer.print("Registers", reader.data.registers);
  printer.print("Parameters", reader.data.parameters);
  printer.print("Parameter values", reader.data.parameter_values);
  printer.print("FBOs", reader.data.fbos);
  printer.print("FBO Attachments", reader.data.fbo_attachments);
  printer.print("Shaders", reader.data.shaders);
  printer.print("Audio buffers", reader.data.audio_buffers);
  printer.print("Ring buffers", reader.data.ring_buffers);
  printer.print("Modulators", reader.data.modulators);

  if (verbose) {
    printer.print("String hashes", reader.data.string_hashes);
    // printer.print("String offsets", reader.data.string_offsets);
    printer.print("String values", reader.data.strings);
  }

  return 0;
};
