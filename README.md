# TERMBOX2-NIF

> **[0.3.1]** Now supports Unicode terminal titles, new NIFs (`tb_set_title/1`, `tb_set_position/2`), and improved error handling. See CHANGELOG for details.

[![Contributors](https://img.shields.io/github/contributors/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/graphs/contributors)
[![Forks](https://img.shields.io/github/forks/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/network/members)
[![Issues](https://img.shields.io/github/issues/Hydepwns/termbox2-nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/issues)
[![Hex.pm Version](https://img.shields.io/hexpm/v/termbox2_nif.svg?style=for-the-badge)](https://hex.pm/packages/termbox2_nif)
[![License](https://img.shields.io/hexpm/l/termbox2_nif.svg?style=for-the-badge)](https://github.com/Hydepwns/termbox2-nif/blob/master/LICENSE)

---

## About The Project

A modern, cross-platform BEAM (Erlang/Elixir/Gleam) wrapper for the [termbox2](https://github.com/termbox/termbox2) terminal UI library.

- **Erlang NIF API**: Low-level, direct access to termbox2 from Erlang.
- **Elixir wrapper**: Ergonomic, idiomatic Elixir API for TUI development.
- **Gleam wrapper**: Type-safe, functional API for Gleam projects.

**Published on Hex.pm:** [https://hex.pm/packages/termbox2_nif](https://hex.pm/packages/termbox2_nif)

> _Note: The repository name was changed because the original "termbox2" name was already taken by the upstream C project. This package provides BEAM/Elixir/Erlang/Gleam bindings for termbox2, not the original C library itself._

## Wrappers Summary

- **Erlang NIF:** Low-level API ([termbox2_nif/](termbox2_nif/))
- **Elixir:** Ergonomic TUI API ([wrappers/elixir/](wrappers/elixir/README.md))
- **Gleam:** Type-safe TUI API ([wrappers/gleam/](wrappers/gleam/README.md))

> NIF binary: built to `termbox2_nif/priv/`. Wrappers use this automatically via the unified workflow.

---

## Table of Contents

- [Getting Started](#getting-started)
- [Unified Build, Test, and Clean (Recommended)](#unified-build-test-and-clean-recommended)
- [Troubleshooting FAQ](#troubleshooting-faq)
- [Usage](#usage)
  - [Erlang Quickstart](#erlang-quickstart)
  - [Elixir Quickstart](#elixir-quickstart)
  - [Gleam Quickstart](#gleam-quickstart)
- [Advanced/Wrapper-Specific Docs](#advancedwrapper-specific-docs)
- [Roadmap](#roadmap)

---

## Getting Started

### Prerequisites

- Erlang/OTP 22+
- `make`
- C compiler (GCC/Clang)

### Installation

#### Erlang

Add to your `rebar.config`:

```erlang
{deps, [
  {termbox2_nif, {path, "termbox2_nif"}}
]}.
```

Fetch and compile:

```sh
rebar3 get-deps
rebar3 compile
```

#### Elixir

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:termbox2, path: "wrappers/elixir"}, {:termbox2_nif, "~> 0.3.0"}
  ]
end
```

Then run:

```sh
mix deps.get
```

See [wrappers/elixir/README.md](wrappers/elixir/README.md) for full instructions.

#### Gleam

See [wrappers/gleam/README.md](wrappers/gleam/README.md) for full instructions.

---

## Unified Build, Test, and Clean (Recommended)

> **Always use these commands from the project root for all development and CI.**

```sh
make build      # Build everything (C, Erlang, Elixir, Gleam)
make test       # Run all tests (Erlang, Elixir, Gleam)
make clean-all  # Clean all build artifacts and outputs
```

Or from a wrapper directory (Elixir or Gleam):

```sh
make build   # Build (delegates to root)
make test    # Test (delegates to root)
make clean   # Clean wrapper build artifacts
```

These commands ensure the correct NIF/BEAM files are always in place. **Do not manually copy or symlink NIF/BEAM files.**

---

## Troubleshooting FAQ

- **NIF not found:**
  - Run `make build` from the project root.
  - Ensure you have a C compiler installed (GCC/Clang).
  - Check that you have permission to write to the build directories.
- **Erlang header issues:**
  - If you see errors about missing `erl_nif.h`, ensure you have Erlang development headers installed.
  - The build system will automatically detect your Erlang installation path.
  - For custom Erlang installations, you can set `CFLAGS` environment variable to include your Erlang include path.
- **Permissions errors:**
  - If you see permission errors, try `chmod +x rebar3` or run as a user with appropriate rights.
- **Platform-specific notes:**
  - **macOS:** You may need to run `xcode-select --install` to install developer tools.
  - **Linux:** Ensure `build-essential` or equivalent is installed.
  - **Windows:** Use WSL or a compatible build environment.

---

## Windows Build Instructions

- Install [MSYS2](https://www.msys2.org/) or [MinGW-w64](http://mingw-w64.org/)
- Install Erlang/OTP for Windows
- Open MSYS2/MinGW shell
- Run:

  ```sh
  make build
  ```

- The NIF will be built as a DLL and placed in the priv/ directory

---

## Usage

### Erlang Quickstart

```erlang
%% Define default attributes (foreground/background)
-define(TB_DEFAULT, 0).

ok = termbox2_nif:tb_init(),
ok = termbox2_nif:tb_clear(),
Str = "Hello, termbox!",
X = 1, Y = 1,
Fg = ?TB_DEFAULT, Bg = ?TB_DEFAULT,
ok = termbox2_nif:tb_print(X, Y, Fg, Bg, Str),
%% New in 0.3.1: Set terminal title (Unicode supported)
ok = termbox2_nif:tb_set_title("测试Unicode标题"),
%% New in 0.3.1: Set terminal window position (with bounds checking)
ok = termbox2_nif:tb_set_position(100, 100),
ok = termbox2_nif:tb_present(),
{ok, _Type, _Mod, _KeyOrChar} = termbox2_nif:tb_poll_event(),
ok = termbox2_nif:tb_shutdown().
```

### Elixir Quickstart

```elixir
# In your mix.exs, add:
# {:termbox2, path: "wrappers/elixir"}, {:termbox2_nif, "~> 0.3.0"}

:ok = Termbox2.init()
:ok = Termbox2.set_cell(0, 0, ?A, :red, :default)
:ok = Termbox2.present()
:ok = Termbox2.shutdown()
```

### Gleam Quickstart

```gleam
import termbox2_gleam
import termbox2_gleam.{Red, Default, set_cell_friendly, print_friendly}

pub fn main() {
  case termbox2_gleam.init() {
    Ok(_) -> {
      set_cell_friendly(0, 0, 65, Red, Default)
      print_friendly(1, 1, Red, Default, "Hello, world!")
      termbox2_gleam.present()
      termbox2_gleam.shutdown()
    }
    Error(e) -> {
      io.println("Failed to initialize termbox2: " <> e)
    }
  }
}
```

---

## Advanced/Wrapper-Specific Docs

- [Erlang NIF API](c_src/termbox2_nif.c)
- [Elixir wrapper documentation](wrappers/elixir/README.md)
- [Gleam wrapper documentation](wrappers/gleam/README.md)

---

## Roadmap

- [ ] More advanced TUI helpers
- [ ] Windows support improvements
- [ ] More property-based tests
