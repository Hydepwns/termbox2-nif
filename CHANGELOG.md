# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.9] - 2025-05-28

### Added

- Automated fetching of the termbox2 C source in the build process:
  - `c_src/Makefile` now auto-fetches the termbox2 source if missing before building.
  - `rebar.config.script` pre_hooks now ensure the termbox2 source is cloned if missing before compilation.

### Changed

- Refactored Makefile to treat termbox2 as header-only and removed unnecessary source file rules.

## [0.1.8]

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

## [0.2.0] - 2025-06-05

### Changed

- **Improved NIF Loading Mechanism:**
  - Refactored the NIF loading logic in `src/termbox2_nif.erl` for enhanced robustness and clarity.
  - Prioritizes using `code:priv_dir/1` to accurately locate the NIF within standard Mix/Rebar3 project build directories (e.g., `_build/dev/lib/my_app/priv`). This resolves issues where the NIF might not be found in certain contexts like `mix test`.
  - The `TERMBOX2_NIF_PATH` environment variable is retained as a flexible override for custom NIF locations.
  - Eliminated attempts to load the NIF from ambiguous relative paths (such as `priv/` or the current working directory `./`), which previously caused confusing `dlopen` error messages even when the NIF would eventually load.
  - Simplified the process of trying different NIF library extensions (e.g., `.so`, `.dylib`), making the path resolution cleaner.
  - These changes lead to more reliable NIF discovery, better alignment with Erlang/Elixir ecosystem practices for NIFs, and a reduction in console noise from failed load attempts.

## [0.2.1] - 2024-03-19

### Fixed

- Improved Erlang header path detection in build system:
  - Updated `rebar.config.script` to dynamically detect Erlang include paths
  - Enhanced `c_src/Makefile` to properly handle Erlang header dependencies
  - Resolved issues with `erl_nif.h` not being found during compilation

## [0.3.0] - 2024-06-07

### Added

- Tag for new minor release with the following grouped improvements:
  - Build system enhancements: dynamic Erlang include path detection, improved compiler flags, and platform-specific settings
  - Documentation updates: troubleshooting for Erlang header issues, improved usage instructions
  - NIF and test improvements: better error handling, enhanced test suite
  - Development environment tweaks: updated .tool-versions and VSCode settings

## [0.3.1] - 2024-06-08

### Added
- Exported and documented new NIF functions: `tb_set_title/1` and `tb_set_position/2`.
- Unicode support for terminal titles (accepts both binaries and strings).
- Bounds checking for `tb_set_position/2`.

### Changed
- Standardized NIF return values for `tb_set_title/1` and `tb_set_position/2` to always return `{ok, _}` on success and `{error, _}` on failure, matching test expectations.
- Fixed NIF module name mismatch in `ERL_NIF_INIT` macro.
- Removed redundant NIF loading from the compatibility module (`termbox2`).

### Fixed
- All tests now pass on all supported platforms.
