#include "hans/tasks.hpp"
#include <thread>

using namespace hans;

TaskQueue::TaskQueue() : _task_ids(0), _block(false) {
  _stop.store(false);
}

TaskQueue::task_id TaskQueue::async(Tag tag, std::function<void(void)> thunk) {
  auto id = _task_ids++;

  {
    std::lock_guard<std::mutex> lock(_mutex);
    if (!_block) {
      _tasks.push_back({id, tag, thunk});
    }
  }

  return id;
}

void TaskQueue::block_and_clear() {
  std::lock_guard<std::mutex> lock(_mutex);
  _block = true;
  _tasks.clear();
}

void TaskQueue::unblock() {
  std::lock_guard<std::mutex> lock(_mutex);
  _block = false;
}

void TaskQueue::run_forever() {
  std::chrono::microseconds resolution(100);

  while (!_stop.load()) {
    bool empty = true;
    Task task;

    {
      std::lock_guard<std::mutex> lock(_mutex);
      if (!_tasks.empty()) {
        empty = false;
        task = _tasks.front();
        _tasks.pop_front();
      }
    }

    if (!empty) {
      task.thunk();
    } else {
      std::this_thread::sleep_for(resolution);
    }
  }
}

void TaskQueue::stop() {
  _stop.store(true);
}
