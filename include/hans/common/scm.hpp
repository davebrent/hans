#ifndef HANS_COMMON_SCM_H
#define HANS_COMMON_SCM_H

#include <libguile.h>
#include "hans/common/hasher.hpp"

extern "C" {
void scm_init_common_module();
}

namespace hans {
namespace common {

SCM hans_hash(SCM str, SCM hex);

} // namespace common
} // namespace hans

#endif // HANS_COMMON_SCM_H
