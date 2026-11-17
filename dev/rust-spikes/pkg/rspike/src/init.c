
// clang-format sorts includes unless SortIncludes: Never. However, the ordering
// does matter here. So, we need to disable clang-format for safety.

// clang-format off
#include <stdint.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
// clang-format on

#include "rust/api.h"

static uintptr_t TAGGED_POINTER_MASK = (uintptr_t)1;

SEXP handle_result(SEXP res_) {
    uintptr_t res = (uintptr_t)res_;

    // An error is indicated by tag.
    if ((res & TAGGED_POINTER_MASK) == 1) {
        // Remove tag
        SEXP res_aligned = (SEXP)(res & ~TAGGED_POINTER_MASK);

        // Currently, there are two types of error cases:
        //
        //   1. Error from Rust code
        //   2. Error from R's C API, which is caught by R_UnwindProtect()
        //
        if (TYPEOF(res_aligned) == CHARSXP) {
            // In case 1, the result is an error message that can be passed to
            // Rf_errorcall() directly.
            Rf_errorcall(R_NilValue, "%s", CHAR(res_aligned));
        } else {
            // In case 2, the result is the token to restart the
            // cleanup process on R's side.
            R_ContinueUnwind(res_aligned);
        }
    }

    return (SEXP)res;
}

SEXP savvy_rs_call_closure__impl(SEXP c_arg__f, SEXP c_arg__times) {
    SEXP res = savvy_rs_call_closure__ffi(c_arg__f, c_arg__times);
    return handle_result(res);
}

SEXP savvy_rs_call_r__impl(SEXP c_arg__f, SEXP c_arg__x) {
    SEXP res = savvy_rs_call_r__ffi(c_arg__f, c_arg__x);
    return handle_result(res);
}

SEXP savvy_rs_error__impl(SEXP c_arg__msg) {
    SEXP res = savvy_rs_error__ffi(c_arg__msg);
    return handle_result(res);
}

SEXP savvy_rs_noop__impl(void) {
    SEXP res = savvy_rs_noop__ffi();
    return handle_result(res);
}

SEXP savvy_rs_panic__impl(SEXP c_arg__msg) {
    SEXP res = savvy_rs_panic__ffi(c_arg__msg);
    return handle_result(res);
}

SEXP savvy_rs_spin__impl(SEXP c_arg__seconds) {
    SEXP res = savvy_rs_spin__ffi(c_arg__seconds);
    return handle_result(res);
}

SEXP savvy_rs_tax_run__impl(SEXP c_arg__typ, SEXP c_arg__income, SEXP c_arg__risk, SEXP c_arg__nb_off, SEXP c_arg__nb_idx, SEXP c_arg__rate, SEXP c_arg__ticks) {
    SEXP res = savvy_rs_tax_run__ffi(c_arg__typ, c_arg__income, c_arg__risk, c_arg__nb_off, c_arg__nb_idx, c_arg__rate, c_arg__ticks);
    return handle_result(res);
}

SEXP savvy_rs_tax_sweep__impl(SEXP c_arg__typ, SEXP c_arg__income, SEXP c_arg__risk, SEXP c_arg__nb_off, SEXP c_arg__nb_idx, SEXP c_arg__rates, SEXP c_arg__seeds, SEXP c_arg__ticks, SEXP c_arg__threads) {
    SEXP res = savvy_rs_tax_sweep__ffi(c_arg__typ, c_arg__income, c_arg__risk, c_arg__nb_off, c_arg__nb_idx, c_arg__rates, c_arg__seeds, c_arg__ticks, c_arg__threads);
    return handle_result(res);
}

SEXP savvy_Program_eval__impl(SEXP self__, SEXP c_arg__env, SEXP c_arg__n) {
    SEXP res = savvy_Program_eval__ffi(self__, c_arg__env, c_arg__n);
    return handle_result(res);
}

SEXP savvy_Program_new__impl(SEXP c_arg__ir) {
    SEXP res = savvy_Program_new__ffi(c_arg__ir);
    return handle_result(res);
}

SEXP savvy_Store_chr_col__impl(SEXP self__) {
    SEXP res = savvy_Store_chr_col__ffi(self__);
    return handle_result(res);
}

SEXP savvy_Store_col__impl(SEXP self__, SEXP c_arg__j) {
    SEXP res = savvy_Store_col__ffi(self__, c_arg__j);
    return handle_result(res);
}

SEXP savvy_Store_new__impl(SEXP c_arg__n, SEXP c_arg__k) {
    SEXP res = savvy_Store_new__ffi(c_arg__n, c_arg__k);
    return handle_result(res);
}

SEXP savvy_Store_nrow__impl(SEXP self__) {
    SEXP res = savvy_Store_nrow__ffi(self__);
    return handle_result(res);
}

SEXP savvy_Store_set_col__impl(SEXP self__, SEXP c_arg__j, SEXP c_arg__x) {
    SEXP res = savvy_Store_set_col__ffi(self__, c_arg__j, c_arg__x);
    return handle_result(res);
}

SEXP savvy_Store_step__impl(SEXP self__, SEXP c_arg__rng) {
    SEXP res = savvy_Store_step__ffi(self__, c_arg__rng);
    return handle_result(res);
}


static const R_CallMethodDef CallEntries[] = {
    {"savvy_rs_call_closure__impl", (DL_FUNC) &savvy_rs_call_closure__impl, 2},
    {"savvy_rs_call_r__impl", (DL_FUNC) &savvy_rs_call_r__impl, 2},
    {"savvy_rs_error__impl", (DL_FUNC) &savvy_rs_error__impl, 1},
    {"savvy_rs_noop__impl", (DL_FUNC) &savvy_rs_noop__impl, 0},
    {"savvy_rs_panic__impl", (DL_FUNC) &savvy_rs_panic__impl, 1},
    {"savvy_rs_spin__impl", (DL_FUNC) &savvy_rs_spin__impl, 1},
    {"savvy_rs_tax_run__impl", (DL_FUNC) &savvy_rs_tax_run__impl, 7},
    {"savvy_rs_tax_sweep__impl", (DL_FUNC) &savvy_rs_tax_sweep__impl, 9},
    {"savvy_Program_eval__impl", (DL_FUNC) &savvy_Program_eval__impl, 3},
    {"savvy_Program_new__impl", (DL_FUNC) &savvy_Program_new__impl, 1},
    {"savvy_Store_chr_col__impl", (DL_FUNC) &savvy_Store_chr_col__impl, 1},
    {"savvy_Store_col__impl", (DL_FUNC) &savvy_Store_col__impl, 2},
    {"savvy_Store_new__impl", (DL_FUNC) &savvy_Store_new__impl, 2},
    {"savvy_Store_nrow__impl", (DL_FUNC) &savvy_Store_nrow__impl, 1},
    {"savvy_Store_set_col__impl", (DL_FUNC) &savvy_Store_set_col__impl, 3},
    {"savvy_Store_step__impl", (DL_FUNC) &savvy_Store_step__impl, 2},
    {NULL, NULL, 0}
};

void R_init_rspike(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    // Functions for initialization, if any.

}
