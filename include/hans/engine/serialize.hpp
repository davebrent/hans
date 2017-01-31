#ifndef HANS_ENGINE_SERIALIZE_H_
#define HANS_ENGINE_SERIALIZE_H_

#include <cereal/archives/portable_binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include "hans/engine/engine.hpp"
#include "hans/engine/primitives.hpp"

namespace hans {

#define C(T) cereal::make_nvp(#T, d.T)

template <class Archive>
void serialize(Archive& ar, Settings& d) {
  ar(C(channels), C(samplerate), C(blocksize), C(width), C(height));
}

template <class Archive>
void serialize(Archive& ar, Plugins& d) {
  ar(C(filepaths));
}

template <class Archive>
void serialize(Archive& ar, ObjectDef& d) {
  ar(C(id), C(name));
}

template <class Archive>
void serialize(Archive& ar, Register& d) {
  ar(C(object), C(type), C(graph), C(index), C(bin), C(readonly));
}

template <class Archive>
void serialize(Archive& ar, Argument& d) {
  ar(C(name), C(type), C(number), C(boolean), C(string));
}

template <class Archive>
void serialize(Archive& ar, Parameter& d) {
  ar(C(object), C(name), C(size), C(offset));
}

template <class Archive>
void serialize(Archive& ar, RingBuffer& d) {
  ar(C(producer), C(name), C(index));
}

template <class Archive>
void serialize(Archive& ar, Frame& d) {
  ar(C(width), C(height));
}

namespace audio {
template <class Archive>
void serialize(Archive& ar, audio::Buffer& d) {
  ar(C(object), C(name), C(channels), C(size));
}
}

namespace graphics {
template <class Archive>
void serialize(Archive& ar, graphics::Shader& d) {
  ar(C(type), C(name), C(code));
}

template <class Archive>
void serialize(Archive& ar, graphics::FBO::Attachment& d) {
  ar(C(type), C(width), C(height), C(components));
}

template <class Archive>
void serialize(Archive& ar, graphics::FBO& d) {
  ar(C(object), C(stencil_buffer), C(start), C(end));
}
}

template <class Archive>
void serialize(Archive& ar, Strings& d) {
  ar(C(buffer), C(hashes), C(lengths));
}

template <class Archive>
void serialize(Archive& ar, Arguments& d) {
  ar(C(arguments), C(lengths), C(offsets));
}

template <class Archive>
void serialize(Archive& ar, Recordings& d) {
  ar(C(offsets), C(lengths), C(values));
}

template <class Archive>
void serialize(Archive& ar, Range& d) {
  ar(C(start), C(end));
}

template <class Archive>
void serialize(Archive& ar, Graphs& d) {
  ar(C(objects), C(states), C(indices), C(ranges));
}

template <class Archive>
void serialize(Archive& ar, Programs& d) {
  ar(C(names), C(audio), C(graphics));
}

template <class Archive>
void serialize(Archive& ar, Parameters& d) {
  ar(C(split), C(handles), C(buffer));
}

template <class Archive>
void serialize(Archive& ar, Modulator& d) {
  ar(C(source), C(destination), C(offset), C(scale));
}

template <class Archive>
void serialize(Archive& ar, Modulation::Thread& d) {
  ar(C(local), C(cross));
}

template <class Archive>
void serialize(Archive& ar, Modulation& d) {
  ar(C(audio), C(graphics));
}

template <class Archive>
void serialize(Archive& ar, EngineData& d) {
  ar(C(settings), C(strings), C(plugins), C(parameters), C(programs),
     C(modulators), C(registers), C(ring_buffers), C(shaders), C(fbos),
     C(fbos_attachments), C(audio_buffers), C(recordings));
}

namespace engine {
template <class Archive>
void serialize(Archive& ar, Engine& d) {
}
}

#undef C

} // namespace hans

#endif // HANS_ENGINE_SERIALZIE_H_
