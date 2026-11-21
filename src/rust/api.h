SEXP savvy_rs_bernoulli__ffi(SEXP c_arg__n, SEXP c_arg__p);
SEXP savvy_rs_copy_doubles__ffi(SEXP c_arg__x);
SEXP savvy_rs_draw__ffi(SEXP c_arg__kind, SEXP c_arg__n, SEXP c_arg__a, SEXP c_arg__b);
SEXP savvy_rs_group_sum__ffi(SEXP c_arg__x, SEXP c_arg__g, SEXP c_arg__ngroups, SEXP c_arg__na_rm);
SEXP savvy_rs_info__ffi(void);
SEXP savvy_rs_native_exec__ffi(SEXP c_arg__desc, SEXP c_arg__state);
SEXP savvy_rs_nb_fold__ffi(SEXP c_arg__mean, SEXP c_arg__x, SEXP c_arg__cols, SEXP c_arg__cnt);
SEXP savvy_rs_noop__ffi(void);
SEXP savvy_rs_pow__ffi(SEXP c_arg__x, SEXP c_arg__y);
SEXP savvy_rs_reduce__ffi(SEXP c_arg__op, SEXP c_arg__x, SEXP c_arg__na_rm);
SEXP savvy_rs_rx_reason__ffi(void);
SEXP savvy_rs_sample_int__ffi(SEXP c_arg__n, SEXP c_arg__size, SEXP c_arg__replace);
SEXP savvy_rs_stack_ticks__ffi(SEXP c_arg__pieces, SEXP c_arg__ticks);
SEXP savvy_rs_test_error__ffi(SEXP c_arg__msg);
SEXP savvy_rs_test_panic__ffi(SEXP c_arg__msg, SEXP c_arg__worker);
SEXP savvy_rs_test_rng_fail__ffi(SEXP c_arg__n, SEXP c_arg__how);

// methods and associated functions for AgentStore
SEXP savvy_AgentStore_assemble__ffi(SEXP self__);
SEXP savvy_AgentStore_export__ffi(SEXP self__);
SEXP savvy_AgentStore_load__ffi(SEXP self__, SEXP c_arg__groups);
SEXP savvy_AgentStore_new__ffi(SEXP c_arg__groups, SEXP c_arg__special, SEXP c_arg__use_mirror);
SEXP savvy_AgentStore_recorded_groups__ffi(SEXP self__, SEXP c_arg__k);
SEXP savvy_AgentStore_set_col__ffi(SEXP self__, SEXP c_arg__group, SEXP c_arg__name, SEXP c_arg__values);
SEXP savvy_AgentStore_snapshot__ffi(SEXP self__, SEXP c_arg__tick, SEXP c_arg__keep);
SEXP savvy_AgentStore_stats__ffi(SEXP self__);
SEXP savvy_AgentStore_ticks__ffi(SEXP self__);

// methods and associated functions for RunLoop
SEXP savvy_RunLoop_new__ffi(SEXP c_arg__kinds, SEXP c_arg__prep, SEXP c_arg__seg, SEXP c_arg__tick, SEXP c_arg__rng, SEXP c_arg__ctx, SEXP c_arg__mprep);
SEXP savvy_RunLoop_run__ffi(SEXP self__, SEXP c_arg__state, SEXP c_arg__ticks, SEXP c_arg__need, SEXP c_arg__has, SEXP c_arg__keep_t, SEXP c_arg__logs);
SEXP savvy_RunLoop_stats__ffi(SEXP self__);
SEXP savvy_RunLoop_times__ffi(SEXP self__);

// methods and associated functions for RxProgram
SEXP savvy_RxProgram_draws__ffi(SEXP self__);
SEXP savvy_RxProgram_eval__ffi(SEXP self__, SEXP c_arg__inputs, SEXP c_arg__n, SEXP c_arg__out_n, SEXP c_arg__stacked);
SEXP savvy_RxProgram_new__ffi(SEXP c_arg__rules, SEXP c_arg__writes, SEXP c_arg__fns);
