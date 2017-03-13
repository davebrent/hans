#include "worker_thread.hpp"

using namespace gui;

WorkerThread::WorkerThread(hans::TaskQueue& tasks, QThread* parent)
    : QThread(parent), _tasks(tasks) {
}

void WorkerThread::run() {
  _tasks.run_forever();
}
