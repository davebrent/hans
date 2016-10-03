#include <libguile.h>
#include "hans/common/hasher.hpp"

using namespace hans;

static SCM hans_hash(SCM str, SCM hex) {
  auto data = scm_to_locale_string(str);
  auto hash = common::hasher(data);
  free(data);

  auto value = scm_from_uint64(hash);
  if (scm_is_true(scm_boolean_p(hex)) == 1 && scm_is_true(hex) == 1) {
    auto prefix = scm_from_locale_string("0x");
    auto suffix = scm_number_to_string(value, scm_from_int(16));
    return scm_string_append(scm_list_2(prefix, suffix));
  }

  return value;
}

extern "C" {
void scm_init_common_module() {
  scm_c_define_gsubr("hans-hash", 1, 1, 0, (scm_t_subr)hans_hash);
}
}
