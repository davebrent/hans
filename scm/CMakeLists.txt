set(SCM_FILES
  compiler/passes/assign-graph-id.scm
  compiler/passes/backend.scm
  compiler/passes/normalize-args.scm
  compiler/passes/configure-objects.scm
  compiler/passes/register-allocation.scm
  compiler/passes/resolve-library-paths.scm
  compiler/passes/topological-sort.scm
  compiler/passes/validate-connections.scm
  compiler/passes/validate-modulators.scm
  compiler/passes/validate-plugins.scm
  compiler/passes/validate-shaders.scm
  compiler/shared.scm
  connect/elektron.scm
  common.scm
  compiler.scm
  connect.scm
  engine.scm
  media.scm
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
