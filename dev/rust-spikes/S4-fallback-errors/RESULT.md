# S4: R fallback embedding and error transport

Date 2026-11-17. macOS arm64, R 4.6.1, rustc/cargo 1.100.0, savvy 0.11.0 with
`panic = "unwind"`, tidyABM `77ae511`. Script `s4.R`, log `results/s4.txt`.

## Results: 14 / 14 PASS

| check | result |
|---|---|
| Rust kernel (draws from R's RNG) alternating with an R-evaluated `if_else` rule that also draws, 50 ticks on one store, vs the same in plain R | identical values and one shared RNG stream |
| `rlang::abort(class = "my_error", payload = 42)` inside R code called from Rust | class vector, message and custom field reach the R caller unchanged (savvy `R_UnwindProtect` + `R_ContinueUnwind`) |
| base error "object 'monney' not found" from Rust-called R | same class and message as in plain R |
| that error through the engine's own `rethrow_step_error()` | same classes (`tidyABM_missing_column / tidyABM_step_error / ...`) and the **identical full cli message** (hints included) as when the rule runs in plain R |
| `warning()` in Rust-called R + `muffleWarning` | handler sees it, restart resumes, value returned |
| a restart established in R, invoked inside Rust-called R | unwinds through the Rust frames cleanly |
| typed Rust error (`Err(savvy_err!(..))`) | R error with that message (class `simpleError`; design rule 1 keeps user-facing classes in R via `abm_abort()`) |
| `panic!` on the main thread | R error "panic happened", session survives |
| panic inside a `std::thread::scope` worker | propagated to the main thread, caught, R error; session survives and later calls work |
| Ctrl-C (SIGINT) during a 10 s Rust loop polling `R_CheckUserInterrupt` (through `unwind_protect`) every ~1 ms | `interrupt` condition caught in R **0.01 s** after the signal; the same session goes on (`ALIVE 2`) |
| `gctorture(TRUE)` over store copy in/out, character materialisation, closures called from Rust, IR evaluation, the tax kernel, an R error and a panic | identical results, no crash |

## Findings that change the design a little

1. **savvy's panic hook prints from whichever thread panicked.** A panic in a worker
   thread printed "panic occured! ..." through savvy's hook, which writes with R's
   console functions: that is an R API call from a non-main thread (forbidden; it
   happened to work here). P1/P6 must install their own panic hook (`std::panic::
   set_hook`) that only records the message (no R calls) and let the main thread
   raise the R error. Also the hook's console output is noisy for users; the engine
   should print nothing and attach the panic message to a `tidyABM_internal_error`.
2. A panic's message reaches R only as "panic happened" (the detail goes to the
   console). Same fix: keep the payload and raise it through `abm_abort()`.
3. The typed-error path (`Err`) gives `simpleError`; per design rule 1 the ffi layer
   should return a code + fields and let R raise the classed condition, exactly as
   PLAN.md says. No change.

## Verdict against PLAN.md

**PASS.** The kill criterion ("an R error or a panic that cannot be carried across
without losing the condition class or crashing the session") is not hit: classed R
conditions survive the boundary bit for bit, panics (main thread and worker) become R
errors, interrupts are prompt, GC torture is clean. Go for the hybrid per-rule fallback
design with savvy.
