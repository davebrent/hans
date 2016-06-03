add_library(hans-compiler-bindings SHARED
    Compiler.cpp)

target_link_libraries(hans-compiler-bindings
  hans
  hansmodule
  ${GUILE_LIBRARIES})

install(TARGETS hans-compiler-bindings DESTINATION lib)
