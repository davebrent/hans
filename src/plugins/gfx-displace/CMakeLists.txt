pkg_search_module(NANOVG QUIET libnanovg)

if(NANOVG_FOUND)
  include_directories(${NANOVG_INCLUDE_DIRS})
  link_directories(${NANOVG_LIBRARY_DIRS})
  add_library(hans.gfx.displace SHARED main.cpp)

  target_link_libraries(hans.gfx.displace
    hansplugin
    ${NANOVG_LIBRARIES})

  install(TARGETS hans.gfx.displace DESTINATION lib)
endif(NANOVG_FOUND)
