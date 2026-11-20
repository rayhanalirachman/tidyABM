
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

SEXP savvy_rs_bernoulli__impl(SEXP c_arg__n, SEXP c_arg__p) {
    SEXP res = savvy_rs_bernoulli__ffi(c_arg__n, c_arg__p);
    return handle_result(res);
}

SEXP savvy_rs_copy_doubles__impl(SEXP c_arg__x) {
    SEXP res = savvy_rs_copy_doubles__ffi(c_arg__x);
    return handle_result(res);
}

SEXP savvy_rs_draw__impl(SEXP c_arg__kind, SEXP c_arg__n, SEXP c_arg__a, SEXP c_arg__b) {
    SEXP res = savvy_rs_draw__ffi(c_arg__kind, c_arg__n, c_arg__a, c_arg__b);
    return handle_result(res);
}

SEXP savvy_rs_group_sum__impl(SEXP c_arg__x, SEXP c_arg__g, SEXP c_arg__ngroups, SEXP c_arg__na_rm) {
    SEXP res = savvy_rs_group_sum__ffi(c_arg__x, c_arg__g, c_arg__ngroups, c_arg__na_rm);
    return handle_result(res);
}

SEXP savvy_rs_info__impl(void) {
    SEXP res = savvy_rs_info__ffi();
    return handle_result(res);
}

SEXP savvy_rs_native_exec__impl(SEXP c_arg__desc, SEXP c_arg__state) {
    SEXP res = savvy_rs_native_exec__ffi(c_arg__desc, c_arg__state);
    return handle_result(res);
}

SEXP savvy_rs_nb_fold__impl(SEXP c_arg__mean, SEXP c_arg__x, SEXP c_arg__cols, SEXP c_arg__cnt) {
    SEXP res = savvy_rs_nb_fold__ffi(c_arg__mean, c_arg__x, c_arg__cols, c_arg__cnt);
    return handle_result(res);
}

SEXP savvy_rs_noop__impl(void) {
    SEXP res = savvy_rs_noop__ffi();
    return handle_result(res);
}

SEXP savvy_rs_pow__impl(SEXP c_arg__x, SEXP c_arg__y) {
    SEXP res = savvy_rs_pow__ffi(c_arg__x, c_arg__y);
    return handle_result(res);
}

SEXP savvy_rs_reduce__impl(SEXP c_arg__op, SEXP c_arg__x, SEXP c_arg__na_rm) {
    SEXP res = savvy_rs_reduce__ffi(c_arg__op, c_arg__x, c_arg__na_rm);
    return handle_result(res);
}

SEXP savvy_rs_rx_reason__impl(void) {
    SEXP res = savvy_rs_rx_reason__ffi();
    return handle_result(res);
}

SEXP savvy_rs_sample_int__impl(SEXP c_arg__n, SEXP c_arg__size, SEXP c_arg__replace) {
    SEXP res = savvy_rs_sample_int__ffi(c_arg__n, c_arg__size, c_arg__replace);
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

SEXP savvy_rs_test_rng_fail__impl(SEXP c_arg__n, SEXP c_arg__how) {
    SEXP res = savvy_rs_test_rng_fail__ffi(c_arg__n, c_arg__how);
    return handle_result(res);
}

SEXP savvy_AgentStore_assemble__impl(SEXP self__) {
    SEXP res = savvy_AgentStore_assemble__ffi(self__);
    return handle_result(res);
}

SEXP savvy_AgentStore_export__impl(SEXP self__) {
    SEXP res = savvy_AgentStore_export__ffi(self__);
    return handle_result(res);
}

SEXP savvy_AgentStore_load__impl(SEXP self__, SEXP c_arg__groups) {
    SEXP res = savvy_AgentStore_load__ffi(self__, c_arg__groups);
    return handle_result(res);
}

SEXP savvy_AgentStore_new__impl(SEXP c_arg__groups, SEXP c_arg__special, SEXP c_arg__use_mirror) {
    SEXP res = savvy_AgentStore_new__ffi(c_arg__groups, c_arg__special, c_arg__use_mirror);
    return handle_result(res);
}

SEXP savvy_AgentStore_recorded_groups__impl(SEXP self__, SEXP c_arg__k) {
    SEXP res = savvy_AgentStore_recorded_groups__ffi(self__, c_arg__k);
    return handle_result(res);
}

SEXP savvy_AgentStore_set_col__impl(SEXP self__, SEXP c_arg__group, SEXP c_arg__name, SEXP c_arg__values) {
    SEXP res = savvy_AgentStore_set_col__ffi(self__, c_arg__group, c_arg__name, c_arg__values);
    return handle_result(res);
}

SEXP savvy_AgentStore_snapshot__impl(SEXP self__, SEXP c_arg__tick, SEXP c_arg__keep) {
    SEXP res = savvy_AgentStore_snapshot__ffi(self__, c_arg__tick, c_arg__keep);
    return handle_result(res);
}

SEXP savvy_AgentStore_stats__impl(SEXP self__) {
    SEXP res = savvy_AgentStore_stats__ffi(self__);
    return handle_result(res);
}

SEXP savvy_AgentStore_ticks__impl(SEXP self__) {
    SEXP res = savvy_AgentStore_ticks__ffi(self__);
    return handle_result(res);
}

SEXP savvy_RunLoop_new__impl(SEXP c_arg__kinds, SEXP c_arg__prep, SEXP c_arg__seg, SEXP c_arg__tick, SEXP c_arg__rng, SEXP c_arg__ctx) {
    SEXP res = savvy_RunLoop_new__ffi(c_arg__kinds, c_arg__prep, c_arg__seg, c_arg__tick, c_arg__rng, c_arg__ctx);
    return handle_result(res);
}

SEXP savvy_RunLoop_run__impl(SEXP self__, SEXP c_arg__state, SEXP c_arg__ticks, SEXP c_arg__need, SEXP c_arg__glog) {
    SEXP res = savvy_RunLoop_run__ffi(self__, c_arg__state, c_arg__ticks, c_arg__need, c_arg__glog);
    return handle_result(res);
}

SEXP savvy_RunLoop_stats__impl(SEXP self__) {
    SEXP res = savvy_RunLoop_stats__ffi(self__);
    return handle_result(res);
}

SEXP savvy_RxProgram_draws__impl(SEXP self__) {
    SEXP res = savvy_RxProgram_draws__ffi(self__);
    return handle_result(res);
}

SEXP savvy_RxProgram_eval__impl(SEXP self__, SEXP c_arg__inputs, SEXP c_arg__n, SEXP c_arg__out_n, SEXP c_arg__stacked) {
    SEXP res = savvy_RxProgram_eval__ffi(self__, c_arg__inputs, c_arg__n, c_arg__out_n, c_arg__stacked);
    return handle_result(res);
}

SEXP savvy_RxProgram_new__impl(SEXP c_arg__rules, SEXP c_arg__writes, SEXP c_arg__fns) {
    SEXP res = savvy_RxProgram_new__ffi(c_arg__rules, c_arg__writes, c_arg__fns);
    return handle_result(res);
}


static const R_CallMethodDef CallEntries[] = {
    {"savvy_rs_bernoulli__impl", (DL_FUNC) &savvy_rs_bernoulli__impl, 2},
    {"savvy_rs_copy_doubles__impl", (DL_FUNC) &savvy_rs_copy_doubles__impl, 1},
    {"savvy_rs_draw__impl", (DL_FUNC) &savvy_rs_draw__impl, 4},
    {"savvy_rs_group_sum__impl", (DL_FUNC) &savvy_rs_group_sum__impl, 4},
    {"savvy_rs_info__impl", (DL_FUNC) &savvy_rs_info__impl, 0},
    {"savvy_rs_native_exec__impl", (DL_FUNC) &savvy_rs_native_exec__impl, 2},
    {"savvy_rs_nb_fold__impl", (DL_FUNC) &savvy_rs_nb_fold__impl, 4},
    {"savvy_rs_noop__impl", (DL_FUNC) &savvy_rs_noop__impl, 0},
    {"savvy_rs_pow__impl", (DL_FUNC) &savvy_rs_pow__impl, 2},
    {"savvy_rs_reduce__impl", (DL_FUNC) &savvy_rs_reduce__impl, 3},
    {"savvy_rs_rx_reason__impl", (DL_FUNC) &savvy_rs_rx_reason__impl, 0},
    {"savvy_rs_sample_int__impl", (DL_FUNC) &savvy_rs_sample_int__impl, 3},
    {"savvy_rs_test_error__impl", (DL_FUNC) &savvy_rs_test_error__impl, 1},
    {"savvy_rs_test_panic__impl", (DL_FUNC) &savvy_rs_test_panic__impl, 2},
    {"savvy_rs_test_rng_fail__impl", (DL_FUNC) &savvy_rs_test_rng_fail__impl, 2},
    {"savvy_AgentStore_assemble__impl", (DL_FUNC) &savvy_AgentStore_assemble__impl, 1},
    {"savvy_AgentStore_export__impl", (DL_FUNC) &savvy_AgentStore_export__impl, 1},
    {"savvy_AgentStore_load__impl", (DL_FUNC) &savvy_AgentStore_load__impl, 2},
    {"savvy_AgentStore_new__impl", (DL_FUNC) &savvy_AgentStore_new__impl, 3},
    {"savvy_AgentStore_recorded_groups__impl", (DL_FUNC) &savvy_AgentStore_recorded_groups__impl, 2},
    {"savvy_AgentStore_set_col__impl", (DL_FUNC) &savvy_AgentStore_set_col__impl, 4},
    {"savvy_AgentStore_snapshot__impl", (DL_FUNC) &savvy_AgentStore_snapshot__impl, 3},
    {"savvy_AgentStore_stats__impl", (DL_FUNC) &savvy_AgentStore_stats__impl, 1},
    {"savvy_AgentStore_ticks__impl", (DL_FUNC) &savvy_AgentStore_ticks__impl, 1},
    {"savvy_RunLoop_new__impl", (DL_FUNC) &savvy_RunLoop_new__impl, 6},
    {"savvy_RunLoop_run__impl", (DL_FUNC) &savvy_RunLoop_run__impl, 5},
    {"savvy_RunLoop_stats__impl", (DL_FUNC) &savvy_RunLoop_stats__impl, 1},
    {"savvy_RxProgram_draws__impl", (DL_FUNC) &savvy_RxProgram_draws__impl, 1},
    {"savvy_RxProgram_eval__impl", (DL_FUNC) &savvy_RxProgram_eval__impl, 5},
    {"savvy_RxProgram_new__impl", (DL_FUNC) &savvy_RxProgram_new__impl, 3},
    {NULL, NULL, 0}
};

void R_init_tidyABM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    // Functions for initialization, if any.

}
