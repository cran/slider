#ifndef SLIDER_ASSIGN_H
#define SLIDER_ASSIGN_H

#include "slider.h"
#include "slider-vctrs.h"

// -----------------------------------------------------------------------------

#define ASSIGN_ONE(CONST_DEREF) do { \
  elt = vec_cast(elt, ptype);        \
  p_out[i] = CONST_DEREF(elt)[0];    \
} while (0)

#define ASSIGN_ONE_BARRIER(CONST_DEREF, SET) do { \
  elt = vec_cast(elt, ptype);                     \
  SET(out, i, CONST_DEREF(elt)[0]);               \
} while (0)

// For lists, we don't care what the `elt` is, we just assign it
#define ASSIGN_ONE_LIST(SET) do { \
  SET(out, i, elt);               \
} while (0)

static inline void assign_one_dbl(double* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  ASSIGN_ONE(REAL_RO);
}
static inline void assign_one_int(int* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  ASSIGN_ONE(INTEGER_RO);
}
static inline void assign_one_lgl(int* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  ASSIGN_ONE(LOGICAL_RO);
}
static inline void assign_one_chr(SEXP out, R_len_t i, SEXP elt, SEXP ptype) {
  ASSIGN_ONE_BARRIER(STRING_PTR_RO, SET_STRING_ELT);
}
static inline void assign_one_lst(SEXP out, R_len_t i, SEXP elt, SEXP ptype) {
  ASSIGN_ONE_LIST(SET_VECTOR_ELT);
}

#undef ASSIGN_ONE
#undef ASSIGN_ONE_BARRIER
#undef ASSIGN_ONE_LIST

// -----------------------------------------------------------------------------

#define ASSIGN_LOCS(CTYPE, CONST_DEREF) do {                   \
  elt = PROTECT(vec_cast(elt, ptype));                         \
  const CTYPE value = CONST_DEREF(elt)[0];                     \
                                                               \
  for (R_len_t i = 0; i < size; ++i) {                         \
    p_out[start] = value;                                      \
    ++start;                                                   \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
} while (0)

#define ASSIGN_LOCS_BARRIER(CTYPE, CONST_DEREF, SET) do {      \
  elt = PROTECT(vec_cast(elt, ptype));                         \
  const CTYPE value = CONST_DEREF(elt)[0];                     \
                                                               \
  for (R_len_t i = 0; i < size; ++i) {                         \
    SET(out, start, value);                                    \
    ++start;                                                   \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
} while (0)

// For lists, we don't care what the `elt` is, we just assign it
#define ASSIGN_LOCS_LIST(SET) do {                             \
  for (R_len_t i = 0; i < size; ++i) {                         \
    SET(out, start, elt);                                      \
    ++start;                                                   \
  }                                                            \
} while (0)

static inline void assign_locs_dbl(double* p_out, int start, int size, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(double, REAL_RO);
}
static inline void assign_locs_int(int* p_out, int start, int size, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(int, INTEGER_RO);
}
static inline void assign_locs_lgl(int* p_out, int start, int size, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(int, LOGICAL_RO);
}
static inline void assign_locs_chr(SEXP out, int start, int size, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS_BARRIER(SEXP, STRING_PTR_RO, SET_STRING_ELT);
}
static inline void assign_locs_lst(SEXP out, int start, int size, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS_LIST(SET_VECTOR_ELT);
}

#undef ASSIGN_LOCS
#undef ASSIGN_LOCS_BARRIER
#undef ASSIGN_LOCS_LIST

// -----------------------------------------------------------------------------

#endif
