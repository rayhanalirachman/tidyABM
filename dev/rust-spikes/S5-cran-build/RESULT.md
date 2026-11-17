# S5: CRAN-acceptable build (savvy and extendr hello-world packages)

Date 2026-11-17. macOS 26 arm64, R 4.6.1, Apple clang 17, rustc/cargo 1.100.0,
savvy 0.11.0 + savvy-cli 0.11.0, extendr-api 0.9.0 (pinned `=0.9.0`) + rextendr
0.5.0 template. Packages: `hellosavvy/`, `helloextendr/`. Logs in `results/`.
CRAN policy read on 2026-11-17 (https://cran.r-project.org/web/packages/using_rust.html).

## What was done here (macOS arm64 only)

| item | savvy (`hellosavvy`) | extendr (`helloextendr`) |
|---|---|---|
| template | `savvy-cli init`, then 3 edits: relative `TARGET_DIR`, vendored offline build (`-j 2 --offline`, `CARGO_HOME` inside the package), `panic = "unwind"` + lto | `rextendr::use_extendr()`; vendoring and `-j 2 --offline` already built in (via `NOT_CRAN` unset + `vendor.tar.xz` present) |
| vendoring | `tools/vendor.sh` (`cargo vendor --locked`, xz, writes `inst/AUTHORS` with crate, version, licence) | `rextendr::vendor_pkgs()` (deprecated alias of `vendor_crates()`) |
| crates vendored | 11 (savvy, savvy-ffi, savvy-macro, savvy-bindgen, proc-macro2, quote, syn, unicode-ident, cc, shlex, find-msvc-tools), all MIT or MIT/Apache-2.0 (unicode-ident adds Unicode-3.0) | similar set |
| `vendor.tar.xz` / source tarball | 577 KB / **585 KB** | 578 KB / **586 KB** |
| install log reports rustc version (CRAN asks for it) | yes (`configure`) | yes |
| `R CMD check --as-cran --no-manual` from the tarball, `NOT_CRAN` unset | **0 errors, 1 WARNING, 0 NOTEs** | **0 errors, 1 WARNING, 1 NOTE** (version 0.0.0.9000) |
| the WARNING | "A complete check needs the 'checkbashisms' script" (tool missing on this Mac, not a package problem) | same |
| install from the source folder **in place, path with spaces** (this repo's path) | **fails with savvy's stock template** (`TARGET_DIR = $(CURDIR)/...` splits at the space); **works after the patch** | **fails** when vendored (`mkdir -p $(CARGOTMP)` with `$(CURDIR)`) |

The PDF manual and HTML-tidy checks were skipped (`--no-manual`; no LaTeX / HTML Tidy
installed here); they have nothing to do with Rust.

## CRAN policy points (from the policy page)

Vendor with `cargo vendor` + xz (downloads "should be avoided"), set `-j` to 1 or 2,
report the rustc version in the install log, `SystemRequirements: Cargo (Rust's
package manager), rustc`, test with an old cargo (2+ years), Rust dependencies'
authorship in DESCRIPTION. The page says nothing about a package that builds
without its Rust part when cargo is missing: PLAN.md's R-only `configure` mode (D11)
remains **unverified with CRAN**; ask CRAN or keep it outside CRAN as planned.

## Not run here (blockers; need CI or another machine)

* `R CMD check --as-cran` on ubuntu-latest, macos-15-intel, windows-latest,
  windows-11-arm (the kill criterion is about **Windows x86_64**); R-hub clang-asan /
  valgrind; r-universe webR build. Ready-to-run workflow: `../ci/spikes.yaml` (needs
  a scratch GitHub repository; nothing in tidyABM's repo is touched).
* Oldest rustc that builds it (MSRV): needs a second toolchain. Command for the user:
  `rustup toolchain install 1.81.0` then
  `cd spikes/S5-cran-build/hellosavvy/src/rust && cargo +1.81.0 build --release --offline`
  (after `tar xf vendor.tar.xz` and with `CARGO_HOME` pointing at a dir holding
  `vendor-config.toml` as `config.toml`), or run the `msrv` job in `../ci/spikes.yaml`.
  Note savvy 0.11's lockfile pulls `syn 3.0.10`; its MSRV may exceed 1.81 (unknown).

## Verdict against PLAN.md

**Partial PASS (macOS arm64)**: both bridges produce an offline, vendored, `-j 2`
build that passes `--as-cran` here apart from a missing local tool; tarballs ~0.6 MB
(well under the 5 MB target). **D1 stays savvy**: no savvy blocker found; savvy's
template needs the three small edits above (spaces, vendoring, panic), which tidyABM
would carry in its own `Makevars.in`. extendr's template would also need the spaces
fix. **The kill criterion (Windows x86_64 `--as-cran` clean) is NOT yet tested**:
P1 must not start its CI matrix work assuming it; run `../ci/spikes.yaml` first.
