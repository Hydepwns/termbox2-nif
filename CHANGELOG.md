# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.7] - 2025-05-02

### Added

- Usage example in `README.md`.
- Documentation generation instructions (`rebar3 ex_doc`) in `README.md`.

### Changed

- Updated project version in `src/termbox2_nif.app.src`.

### Fixed

- Committed overlooked macOS NIF loading fixes (`c_src/Makefile`, `c_src/termbox2_nif.c`).

### Removed

- Stopped tracking build artifacts (`_build`, `c_src/obj`, `priv`), generated docs (`doc`), and old test file (`test/termbox2_test.erl`) in git.

## [0.1.6] - 2025-05-02

### Added

- Basic test suite (`termbox2_nif_SUITE.erl`) using Common Test to verify NIF loading (`:on_load`) and basic function availability (`tb_init`, `tb_shutdown`, etc.). This helps prevent `:undef` or `:bad_lib` regressions, especially across different architectures (like macOS ARM).

## [0.1.5] - 2025-04-30

## [0.1.4] - 2025-04-30

## [0.1.3] - 2025-05-02

### Fixed

- Resolved NIF loading issue on macOS ARM (`:bad_lib` error reporting module name mismatch) by aligning the module name in `ERL_NIF_INIT` (`c_src/termbox2_nif.c`) with the Erlang module name (`termbox2_nif`).
- Corrected `c_src/Makefile` to use `.dylib` extension instead of `.so` for shared library output on macOS (`Darwin`).

## [0.1.2] - 2024-04-25

- Initial publish to hex.pm based mostly on garlic0x1's work.

## [0.1.1] - 2024-04-25

- Initial publish attempt.
