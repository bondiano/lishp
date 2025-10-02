#!/usr/bin/env bash

set -eu

# Install tools
cargo clippy --version &>/dev/null || rustup component add clippy
cargo machete --version &>/dev/null || cargo install --locked cargo-machete
cargo sort --version &>/dev/null || cargo install --locked cargo-sort
typos --version &>/dev/null || cargo install --locked typos-cli

cargo fmt --version &>/dev/null || rustup component add rustfmt

# Checks
cargo machete
cargo fmt -- --check
cargo sort -c
cargo test --all-targets --all-features
cargo test --doc
cargo clippy --all-targets --all-features -- -D warnings
