SEXP savvy_rs_call_closure__ffi(SEXP c_arg__f, SEXP c_arg__times);
SEXP savvy_rs_call_r__ffi(SEXP c_arg__f, SEXP c_arg__x);
SEXP savvy_rs_error__ffi(SEXP c_arg__msg);
SEXP savvy_rs_noop__ffi(void);
SEXP savvy_rs_panic__ffi(SEXP c_arg__msg);
SEXP savvy_rs_spin__ffi(SEXP c_arg__seconds);
SEXP savvy_rs_tax_run__ffi(SEXP c_arg__typ, SEXP c_arg__income, SEXP c_arg__risk, SEXP c_arg__nb_off, SEXP c_arg__nb_idx, SEXP c_arg__rate, SEXP c_arg__ticks);
SEXP savvy_rs_tax_sweep__ffi(SEXP c_arg__typ, SEXP c_arg__income, SEXP c_arg__risk, SEXP c_arg__nb_off, SEXP c_arg__nb_idx, SEXP c_arg__rates, SEXP c_arg__seeds, SEXP c_arg__ticks, SEXP c_arg__threads);

// methods and associated functions for Program
SEXP savvy_Program_eval__ffi(SEXP self__, SEXP c_arg__env, SEXP c_arg__n);
SEXP savvy_Program_new__ffi(SEXP c_arg__ir);

// methods and associated functions for Store
SEXP savvy_Store_chr_col__ffi(SEXP self__);
SEXP savvy_Store_col__ffi(SEXP self__, SEXP c_arg__j);
SEXP savvy_Store_new__ffi(SEXP c_arg__n, SEXP c_arg__k);
SEXP savvy_Store_nrow__ffi(SEXP self__);
SEXP savvy_Store_set_col__ffi(SEXP self__, SEXP c_arg__j, SEXP c_arg__x);
SEXP savvy_Store_step__ffi(SEXP self__, SEXP c_arg__rng);
