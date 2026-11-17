#!/bin/sh
# Vendor all crates into src/rust/vendor.tar.xz (+ vendor-config.toml) for an offline,
# CRAN-style build, and list them in inst/AUTHORS. Run from the package root.
set -e
cd src/rust
rm -rf vendor vendor.tar.xz
cargo vendor --locked --versioned-dirs vendor > vendor-config.toml
# cargo vendor writes the source replacement with an absolute path; make it relative
# to the package's src/rust at build time (CARGO_HOME is src/rust/.cargo-home)
sed -i.bak 's|directory = ".*"|directory = "vendor"|' vendor-config.toml && rm -f vendor-config.toml.bak
tar -cJf vendor.tar.xz vendor
mkdir -p ../../inst
{ echo "Rust crates vendored in src/rust/vendor.tar.xz (name, version, licence):"; \
  for d in vendor/*/; do t="$d/Cargo.toml"; n=$(sed -n 's/^name = "\(.*\)"/\1/p' "$t" | head -1); v=$(sed -n 's/^version = "\(.*\)"/\1/p' "$t" | head -1); l=$(sed -n 's/^license = "\(.*\)"/\1/p' "$t" | head -1); echo "$n $v ($l)"; done; } > ../../inst/AUTHORS
rm -rf vendor
ls -la vendor.tar.xz
