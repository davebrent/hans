pkg_search_module(NANOVG QUIET libnanovg)
find_package(Guile QUIET)
include(FindGuile)

if(NANOVG_FOUND)
  if(GUILE_FOUND)
    include_directories(${NANOVG_INCLUDE_DIRS})
    include_directories(${GUILE_INCLUDE_DIRS})
    link_directories(${NANOVG_LIBRARY_DIRS})
    link_directories(${GUILE_LIBRARY_DIRS})

    add_library(hans.gfx.script SHARED
      IMRenderer.cpp
      main.cpp)

    target_link_libraries(hans.gfx.script
      hansplugin
      ${GUILE_LIBRARIES}
      ${NANOVG_LIBRARIES})

    install(TARGETS hans.gfx.script DESTINATION lib)
  endif(GUILE_FOUND)
endif(NANOVG_FOUND)
