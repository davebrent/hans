add_library(hansmedia SHARED
  scm.cpp
  Image.cpp
  Video.cpp)

target_link_libraries(hansmedia
  hanscommon
  ${FREEIMAGE_LIBRARIES})

install(TARGETS hansmedia DESTINATION lib)
