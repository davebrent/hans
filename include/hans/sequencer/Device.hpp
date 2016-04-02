#ifndef HANS_SEQUENCER_DEVICE_H_
#define HANS_SEQUENCER_DEVICE_H_

#include <vector>
#include "hans/sequencer/Backend.hpp"
#include "hans/sequencer/types.hpp"

namespace hans {
namespace sequencer {

class Device {
 public:
  Device(hans::sequencer::Backend* backend);
  ~Device();
  void flush();
  void tick(float delta);
  void send(hans::sequencer::time_event& note);
  void send(hans::sequencer::clock_event& note);
  void send(hans::sequencer::note_event& note);
  void send(hans::sequencer::ctrl_event& event);
  bool pending();

 private:
  hans::sequencer::Backend* m_backend;
  std::vector<hans::sequencer::note_event> m_notes;
  std::vector<hans::sequencer::ctrl_event> m_events;
};

} // namespace sequencer
} // namespace hans

#endif // HANS_SEQUENCER_DEVICE_H_
