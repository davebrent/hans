#ifndef HANS_COMMON_SCM_H
#define HANS_COMMON_SCM_H

#include <libguile.h>
#include "hans/common/Frame.hpp"
#include "hans/common/hasher.hpp"

namespace hans {
namespace common {

void scm_init_common_module();

SCM hans_hash(SCM str, SCM hex);

SCM make_frame(SCM width, SCM height, SCM format);
Frame& scm_to_frame(SCM Frame);

} // namespace common
} // namespace hans

#endif // HANS_COMMON_SCM_H
