#ifndef HANS_SEQ_SERIALIZE_H_
#define HANS_SEQ_SERIALIZE_H_

#include <cereal/archives/xml.hpp>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include "hans/seq/midi.hpp"
#include "hans/seq/sequencer_realtime.hpp"

namespace hans {
namespace seq {

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
void serialize(Archive& ar, SequencerRealtime& d) {
}

#undef C

} // namespace seq
} // namespace hans

#endif // HANS_SEQ_SERIALZIE_H_
