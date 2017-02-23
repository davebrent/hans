if(WEBP_FOUND)
  add_library(hans.gfx.image SHARED main.cpp)
  target_link_libraries(hans.gfx.image
    hansplugin
    ${WEBP_LIBRARIES})
  install(TARGETS hans.gfx.image DESTINATION lib)
endif(WEBP_FOUND)
