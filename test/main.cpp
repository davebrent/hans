#define CATCH_CONFIG_RUNNER
#include <libguile.h>
#include "catch.hpp"

static int argc;
static char** argv;
static int result;

static void* main_resolve(void* data) {
  return data;
}

static void* main_inner(void* data) {
  result = Catch::Session().run(argc, argv);
  return scm_without_guile(main_resolve, nullptr);
}

int main(int argc_, char** argv_) {
  argc = argc_;
  argv = argv_;
  scm_with_guile(main_inner, nullptr);
  return result;
}
