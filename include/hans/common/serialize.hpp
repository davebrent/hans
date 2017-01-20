#ifndef HANS_COMMON_SERIALIZE_H_
#define HANS_COMMON_SERIALIZE_H_

#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include "hans/common/primitives.hpp"

namespace hans {

#define C(T) cereal::make_nvp(#T, d.T)

template <class Archive>
void serialize(Archive& ar, Settings& d) {
  ar(C(channels), C(samplerate), C(blocksize), C(width), C(height));
}

template <class Archive>
void serialize(Archive& ar, Plugin& d) {
  ar(C(filepath));
}

template <class Archive>
void serialize(Archive& ar, ObjectDef& d) {
  ar(C(id), C(type), C(name));
}

template <class Archive>
void serialize(Archive& ar, Chain& d) {
  ar(C(id), C(start), C(end));
}

template <class Archive>
void serialize(Archive& ar, Program& d) {
  ar(C(name), C(graphics), C(audio));
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
void serialize(Archive& ar, Modulator::Port& d) {
  ar(C(object), C(parameter), C(component));
}

template <class Archive>
void serialize(Archive& ar, Modulator& d) {
  ar(C(source), C(dest), C(offset), C(scale));
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
void serialize(Archive& ar, EngineData& d) {
  ar(C(settings), C(strings), C(plugins), C(objects), C(objects_state),
     C(parameters), C(parameters_values), C(programs), C(chains), C(modulators),
     C(registers), C(ring_buffers), C(shaders), C(fbos), C(fbos_attachments),
     C(audio_buffers));
}

#undef C

} // namespace hans

#endif // HANS_COMMON_SERIALZIE_H_
