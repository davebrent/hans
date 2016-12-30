add_library(hanscommon SHARED
  scm.cpp
  DataLoader.cpp
  hasher.cpp
  StringManager.cpp
  LinearAllocator.cpp
  smobs.cpp)

target_link_libraries(hanscommon
  ${GUILE_LIBRARIES})

install(TARGETS hanscommon DESTINATION lib)
