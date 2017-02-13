add_subdirectory(engine)
add_subdirectory(pipeline)
add_subdirectory(plugins)
add_subdirectory(sequencer)

add_executable(hans main.cpp)
target_link_libraries(hans
  hansengine
  hanspipeline
  hanssequencer
  ${LIBEFSW_LIBRARIES})
