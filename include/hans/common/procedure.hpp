#ifndef HANS_SCM_PROCEDURE_H
#define HANS_SCM_PROCEDURE_H

#include <libguile.h>

namespace hans {
namespace scm {

template <SCM (*T)(void)>
void procedure(const char* name) {
  scm_c_define_gsubr(name, 0, 0, 0, (scm_t_subr)T);
}

template <SCM (*T)(void)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, 0, 0, 0, (scm_t_subr)T);
}

template <SCM (*T)(SCM)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, req, opt, rst, (scm_t_subr)T);
}

template <SCM (*T)(SCM, SCM)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, req, opt, rst, (scm_t_subr)T);
}

template <SCM (*T)(SCM, SCM, SCM)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, req, opt, rst, (scm_t_subr)T);
}

template <SCM (*T)(SCM, SCM, SCM, SCM)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, req, opt, rst, (scm_t_subr)T);
}

template <SCM (*T)(SCM, SCM, SCM, SCM, SCM)>
void procedure(const char* name, int req, int opt, int rst) {
  scm_c_define_gsubr(name, req, opt, rst, (scm_t_subr)T);
}

} // namespace scm
} // namespace hans

#endif // HANS_SCM_PROCEDURE_H
