find_package(Qt5Widgets QUIET)
find_package(Qt5OpenGL QUIET)

if(NOT Qt5Widgets_FOUND OR NOT Qt5OpenGL_FOUND)
  return()
endif()

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed.
set(CMAKE_AUTOMOC ON)

add_executable(hans-gui
  engine_widget.cpp
  main.cpp
  main_window.cpp
  parameters_widget.cpp
  sequencer.cpp
  state.cpp
  worker_thread.cpp)

qt5_use_modules(hans-gui Widgets OpenGL)

target_link_libraries(hans-gui
  Qt5::Widgets
  hansplugin
  hans)
