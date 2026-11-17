
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

SEXP savvy_rs_copy_doubles__impl(SEXP c_arg__x) {
    SEXP res = savvy_rs_copy_doubles__ffi(c_arg__x);
    return handle_result(res);
}

SEXP savvy_rs_info__impl(void) {
    SEXP res = savvy_rs_info__ffi();
    return handle_result(res);
}

SEXP savvy_rs_noop__impl(void) {
    SEXP res = savvy_rs_noop__ffi();
    return handle_result(res);
}

SEXP savvy_rs_test_error__impl(SEXP c_arg__msg) {
    SEXP res = savvy_rs_test_error__ffi(c_arg__msg);
    return handle_result(res);
}

SEXP savvy_rs_test_panic__impl(SEXP c_arg__msg, SEXP c_arg__worker) {
    SEXP res = savvy_rs_test_panic__ffi(c_arg__msg, c_arg__worker);
    return handle_result(res);
}


static const R_CallMethodDef CallEntries[] = {
    {"savvy_rs_copy_doubles__impl", (DL_FUNC) &savvy_rs_copy_doubles__impl, 1},
    {"savvy_rs_info__impl", (DL_FUNC) &savvy_rs_info__impl, 0},
    {"savvy_rs_noop__impl", (DL_FUNC) &savvy_rs_noop__impl, 0},
    {"savvy_rs_test_error__impl", (DL_FUNC) &savvy_rs_test_error__impl, 1},
    {"savvy_rs_test_panic__impl", (DL_FUNC) &savvy_rs_test_panic__impl, 2},
    {NULL, NULL, 0}
};

void R_init_tidyABM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    // Functions for initialization, if any.

}
