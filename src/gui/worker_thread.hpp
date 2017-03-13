#ifndef GUI_WORKER_THREAD_H
#define GUI_WORKER_THREAD_H

#include <QThread>
#include <hans/tasks.hpp>

namespace gui {

class WorkerThread : public QThread {
  Q_OBJECT
 public:
  WorkerThread(hans::TaskQueue& tasks, QThread* parent = 0);
  void run() override;

 private:
  hans::TaskQueue& _tasks;
};

} // namespace gui

#endif // GUI_WORKER_THREAD_H
