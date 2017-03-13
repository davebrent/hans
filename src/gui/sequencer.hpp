#ifndef GUI_SEQUENCER_H
#define GUI_SEQUENCER_H

#include <QThread>
#include <hans/primitives.hpp>
#include <hans/sequencer.hpp>
#include <hans/tasks.hpp>

namespace gui {

class Sequencer : public QThread {
  Q_OBJECT
 public:
  Sequencer(hans::TaskQueue& queue, hans::Sequences& sequences,
            QObject* parent = 0);
  void run() override;
  void stop();

 signals:
  void trackEvent(const hans::Track track, float value, bool state);

 public slots:

 private:
  hans::Sequencer _sequencer;
};

} // namespace gui

#endif // GUI_SEQUENCER_H
