# termbox2_gleam

[![Package Version](https://img.shields.io/hexpm/v/termbox2_gleam)](https://hex.pm/packages/termbox2_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/termbox2_gleam/)

## Installation

Add to your Gleam project:

```sh
gleam add termbox2_gleam@1
```

**Requirements:**

- The `termbox2_nif` Erlang NIF must be built and available in your BEAM path. See the main project README for build instructions.
- Works on macOS, Linux, and (experimental) Windows. macOS ARM is supported.

---

## Unified Build, Test, and Clean (Recommended)

> **Always use these commands from the project root for all development and CI.**

```sh
make build      # Build everything (C, Erlang, Elixir, Gleam)
make test       # Run all tests (Erlang, Elixir, Gleam)
make clean-all  # Clean all build artifacts and outputs
```

Or from this directory:

```sh
make build   # Build (delegates to root)
make test    # Test (delegates to root)
make clean   # Clean wrapper build artifacts
```

These commands ensure the correct NIF/BEAM files are always in place. **Do not manually copy or symlink NIF/BEAM files.**

---

## Quick Reference

| Action         | From Project Root      | From Wrapper Directory      |
|----------------|-----------------------|----------------------------|
| Build all      | `make build`          | `make build`               |
| Test all       | `make test`           | `make test`                |
| Clean all      | `make clean-all`      | `make clean`               |

---

## CI Usage

Use these targets in your CI (e.g., GitHub Actions):

```yaml
- run: make build
- run: make test
```

---

## Troubleshooting FAQ

- **NIF not found:**
  - Run `make build` from the project root.
  - Ensure you have a C compiler installed (GCC/Clang).
  - Check that you have permission to write to the build directories.
- **Permissions errors:**
  - If you see permission errors, try `chmod +x rebar3` or run as a user with appropriate rights.
- **Platform-specific notes:**
  - **macOS:** You may need to run `xcode-select --install` to install developer tools.
  - **Linux:** Ensure `build-essential` or equivalent is installed.
  - **Windows:** Use WSL or a compatible build environment.

---

## Usage Example

```gleam
import termbox2_gleam
import termbox2_gleam.{Red, Default, Bold, set_cell_friendly, print_friendly, draw_box, fill_rect, print_centered}

pub fn main() {
  case termbox2_gleam.init() {
    Ok(_) -> {
      termbox2_gleam.clear()
      set_cell_friendly(0, 0, 65, Red, Default) // 'A', fg=Red, bg=Default
      print_friendly(1, 1, Red, Default, "Hello, world!")
      draw_box(0, 0, 10, 5, 35, Red, Default) // Draw a red box
      fill_rect(1, 1, 9, 4, 46, Default, Default) // Fill with '.'
      print_centered(6, "Centered!", Red, Default)
      termbox2_gleam.present()
      termbox2_gleam.shutdown()
    }
    Error(e) -> {
      io.println("Failed to initialize termbox2: " <> e)
    }
  }
}
```

## Ergonomic API Features

- **Enums for colors/attributes:** Use `Red`, `Bold`, etc. for type safety and clarity.
- **Ergonomic helpers:** `set_cell_friendly`, `print_friendly`, and variants accepting lists of colors/attrs.
- **Drawing utilities:** `draw_box`, `fill_rect`, `print_centered` for easy TUI building.
- **Event helpers:** Pattern match on event types, keys, and modifiers with helpers like `decode_event`, `decode_key`, `is_key_event`, `has_mod`.
- **Property-based and edge-case testing:** Extensive test suite covers random and edge-case inputs for all helpers and utilities.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

See the main project README for more details and advanced usage.

NIF binary: built to ../priv/. This is symlinked/copied for dev/test.
