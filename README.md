# TERMBOX2-NIF

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

---

## Table of Contents

- [About The Project](#about-the-project)
- [Getting Started](#getting-started)
- [Usage](#usage)
  - [Erlang Quickstart](#erlang-quickstart)
  - [Elixir Quickstart](#elixir-quickstart)
  - [Gleam Quickstart](#gleam-quickstart)
- [Testing & NIF Loading](#testing--nif-loading)
- [Advanced/Wrapper-Specific Docs](#advancedwrapper-specific-docs)
- [Roadmap](#roadmap)
- [License](#license)
- [Wrappers Summary](#wrappers-summary)

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

See [wrappers/elixir/README.md](wrappers/elixir/README.md) for full instructions.

#### Gleam

See [wrappers/gleam/README.md](wrappers/gleam/README.md) for full instructions.

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
ok = termbox2_nif:tb_present(),
{ok, _Type, _Mod, _KeyOrChar} = termbox2_nif:tb_poll_event(),
ok = termbox2_nif:tb_shutdown().
```

### Elixir Quickstart

```elixir
# In your mix.exs, add:
# {:termbox2, path: "wrappers/elixir"}, {:termbox2_nif, "~> 2.0.0"}

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

## Testing & NIF Loading

- **Erlang:**
  - Run tests: `rebar3 ct`
  - NIF is loaded automatically when built via Makefile or `rebar3`.
- **Elixir:**
  - Run tests: `cd wrappers/elixir && mix test`
  - NIF is loaded automatically; no manual copying needed.
- **Gleam:**
  - Run tests: `make gleam-test` (from project root)
  - This copies the BEAM and sets the code path for NIF loading.
- **All wrappers:**
  - Run all tests: `make build-all`
  - No manual copying or symlinking of BEAM/NIF files is required for any wrapper.

If you encounter NIF loading errors, ensure you have built the project from the root using the Makefile or wrapper-specific instructions.

---

## Advanced/Wrapper-Specific Docs

- [Erlang NIF API](c_src/termbox2_nif.c)
- [Elixir wrapper documentation](wrappers/elixir/README.md)
- [Gleam wrapper documentation](wrappers/gleam/README.md)

---

## Roadmap

- [x] Erlang NIF created
- [x] Gleam wrapper created
- [x] Elixir wrapper created
- [ ] More advanced TUI helpers
- [ ] Windows support improvements
- [ ] More property-based tests

See [open issues](https://github.com/Hydepwns/termbox2-nif/issues) for a full list of proposed features and known issues.

---

## License

Distributed under the MIT License. See `LICENSE` for more information.

---

## Wrappers Summary

- **Erlang NIF:** Low-level API ([termbox2_nif/](termbox2_nif/))
- **Elixir:** Ergonomic TUI API ([wrappers/elixir/](wrappers/elixir/README.md))
- **Gleam:** Type-safe TUI API ([wrappers/gleam/](wrappers/gleam/README.md))

NIF binary: built to `termbox2_nif/priv/`. Wrappers symlink/copy from here as needed.

Build all: `make build-all` (see Makefile for details)
