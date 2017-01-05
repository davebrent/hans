#ifndef HANS_COMMON_SERIALIZE_H_
#define HANS_COMMON_SERIALIZE_H_

#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include "hans/common/types.hpp"

namespace hans {

#define C(T) cereal::make_nvp(#T, d.T)

namespace common {
template <class Archive>
void serialize(Archive& ar, common::Config& d) {
  ar(C(channels), C(samplerate), C(blocksize), C(width), C(height));
}
}

template <class Archive>
void serialize(Archive& ar, Plugin& d) {
  ar(C(filepath));
}

template <class Archive>
void serialize(Archive& ar, ObjectDef& d) {
  ar(C(id), C(type), C(size));
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
  ar(C(producer), C(name), C(offset), C(index));
}

template <class Archive>
void serialize(Archive& ar, Frame& d) {
  ar(C(width), C(height));
}

namespace audio {
template <class Archive>
void serialize(Archive& ar, audio::Buffer& d) {
  ar(C(object), C(name), C(channels), C(size), C(offset));
}
}

namespace graphics {
template <class Archive>
void serialize(Archive& ar, graphics::Shader& d) {
  ar(C(type), C(name), C(code));
}

template <class Archive>
void serialize(Archive& ar, graphics::FBO::Attachment& d) {
  ar(C(type), C(width), C(components));
}

template <class Archive>
void serialize(Archive& ar, graphics::FBO& d) {
  ar(C(object), C(stencil_buffer), C(start), C(end));
}
}

#undef C

} // namespace hans

#endif // HANS_COMMON_SERIALZIE_H_
