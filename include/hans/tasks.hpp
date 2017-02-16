#ifndef HANS_TASKS_H_
#define HANS_TASKS_H_

#include <atomic>
#include <chrono>
#include <deque>
#include <iostream>
#include <mutex>
#include <vector>

namespace hans {

class TaskQueue {
 public:
  using task_id = size_t;

  TaskQueue() : _task_ids(0) {
    _stop.store(false);
  }

  task_id async(size_t tag, std::function<void(void)> thunk) {
    auto id = _task_ids++;

    {
      std::lock_guard<std::mutex> lock(_mutex);
      _tasks.push_back({id, tag, thunk});
    }

    return id;
  }

  void run_forever() {
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

  void stop() {
    _stop.store(true);
  }

 private:
  struct Task {
    size_t id;
    size_t tag;
    std::function<void(void)> thunk;
  };

  size_t _task_ids;
  std::mutex _mutex;
  std::deque<Task> _tasks;
  std::atomic<bool> _stop;
};

} // hans

#endif // HANS_TASKS_H_
