#ifndef HANS_TASKS_H_
#define HANS_TASKS_H_

#include <atomic>
#include <deque>
#include <functional>
#include <mutex>

namespace hans {

class TaskQueue {
 public:
  using task_id = size_t;
  enum Tag {
    SEQUENCER_EVAL,
  };

  TaskQueue();
  task_id async(Tag tag, std::function<void(void)> thunk);
  void run_forever();
  void stop();

 private:
  struct Task {
    size_t id;
    Tag tag;
    std::function<void(void)> thunk;
  };

  size_t _task_ids;
  std::mutex _mutex;
  std::deque<Task> _tasks;
  std::atomic<bool> _stop;
};

} // hans

#endif // HANS_TASKS_H_
