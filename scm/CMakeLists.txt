set(SCM_FILES
  connect/elektron.scm
  common.scm
  compiler.scm
  connect.scm
  engine.scm
  objects.scm
  os.scm
  patcher.scm
  sequencer.scm
  utils.scm)

foreach(SCM_FILE ${SCM_FILES})
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${SCM_FILE}"
    "${SCM_OUTPUT_DIRECTORY}/${SCM_FILE}")
endforeach(SCM_FILE)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/extension.scm.in"
  "${SCM_OUTPUT_DIRECTORY}/extension.scm")

install(DIRECTORY ${SCM_OUTPUT_DIRECTORY} DESTINATION ${GUILE_SITE_DIR})
