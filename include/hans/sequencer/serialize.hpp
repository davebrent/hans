#ifndef HANS_SEQUENCER_SERIALIZE_H_
#define HANS_SEQUENCER_SERIALIZE_H_

#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include "hans/sequencer/midi.hpp"
#include "hans/sequencer/sequencer.hpp"

namespace hans {
namespace sequencer {

#define C(T) cereal::make_nvp(#T, d.T)

template <class Archive>
void serialize(Archive& ar, MidiOut::Device& d) {
  ar(C(name), C(index));
}

template <class Archive>
void serialize(Archive& ar, MidiOut& d) {
  ar(C(devices));
}

template <class Archive>
void serialize(Archive& ar, Cycle& d) {
  ar(C(duration), C(number));
}

template <class Archive>
void serialize(Archive& ar, Sequencer& d) {
}

#undef C

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_SERIALZIE_H_