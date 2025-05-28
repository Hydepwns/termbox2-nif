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

## Testing and NIF Loading

To run all Gleam tests with the correct NIF and code path setup, from the project root run:

```sh
make gleam-test
```

This will automatically:

- Copy the latest `termbox2_nif.beam` into the Gleam wrapper's build output.
- Set the correct code path so the NIF is always found by the BEAM VM.

You no longer need to manually copy or symlink BEAM files for Gleam tests.

## Troubleshooting

- If you see an error about the NIF not loading, ensure you have built the NIF (`rebar3 compile` in the main project) and that the resulting `.so`/`.dylib` is in the `priv/` directory.
- The NIF must be available in the BEAM code path. If running from source, use `make gleam-test` to ensure the correct setup.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

See the main project README for more details and advanced usage.

NIF binary: built to ../priv/. This is symlinked/copied for dev/test.
