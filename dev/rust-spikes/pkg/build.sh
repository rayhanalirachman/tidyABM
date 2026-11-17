#!/bin/bash
# Build and install the spike package rspike into spikes/lib.
# The package path contains spaces, and savvy's Makevars (TARGET_DIR = $(CURDIR)/...)
# breaks on spaces, so we always go through R CMD build (R then compiles in a temp
# dir). The rrng crate (spike S1) is copied in first because a tarball cannot hold a
# path dependency outside the package.
set -eu
export PATH="$HOME/.cargo/bin:$PATH"
P="$(cd "$(dirname "$0")" && pwd)"; S="$P/.."
rm -rf "$P/rspike/src/rust/rrng"; mkdir -p "$P/rspike/src/rust/rrng"
cp -R "$S/S1-rng/rrng/Cargo.toml" "$S/S1-rng/rrng/src" "$P/rspike/src/rust/rrng/"
rm -rf "$P/rspike/src/rust/rrng/src/bin"
(cd "$P/rspike" && savvy-cli update . >/dev/null)
B="${TMPDIR:-/tmp}/rspike-build"; rm -rf "$B"; mkdir -p "$B"; cd "$B"
R CMD build --no-build-vignettes "$P/rspike" >/dev/null
R CMD INSTALL -l "$S/lib" rspike_*.tar.gz > "$S/lib/install.log" 2>&1 || { tail -30 "$S/lib/install.log"; exit 1; }
grep -E "using Rust" "$S/lib/install.log"
echo "installed rspike into $S/lib"
