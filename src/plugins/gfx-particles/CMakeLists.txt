find_package(Eigen3 QUIET)

if(EIGEN3_FOUND)
  include_directories(${EIGEN3_INCLUDE_DIRS})
  add_library(hans.gfx.particles SHARED
    bytecode.cpp
    virtual_machine.cpp
    visualisers.cpp
    main.cpp)
  target_link_libraries(hans.gfx.particles hansplugin)
  install(TARGETS hans.gfx.particles DESTINATION lib)
  add_executable(gfx-particles-test
    bytecode_test.cpp
    virtual_machine_test.cpp
    main_test.cpp)
  target_link_libraries(gfx-particles-test hans.gfx.particles)
endif(EIGEN3_FOUND)
