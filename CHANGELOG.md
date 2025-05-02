# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.8] - YYYY-MM-DD

### Fixed
- Removed non-existent application callback module (`termbox2_nif_app`) from `.app.src` file to prevent runtime errors when used as a dependency.

## [0.1.7] - 2024-07-24

### Added
- Added `README.md` documentation about generating edoc documentation.
- Added basic `.gitignore` file.
- Published package to hex.pm.

### Fixed
- Ensured NIFs load correctly and can be called from Erlang.
- Ensured `edoc` documentation generation works.

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

## [0.1.0] - 2024-07-23

### Added
- Initial setup with basic NIF functions for termbox2.
- Basic tests to verify NIF loading.
