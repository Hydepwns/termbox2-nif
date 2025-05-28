# Termbox2 Elixir

Elixir wrapper for the [`termbox2_nif`](https://hex.pm/packages/termbox2_nif) NIF library, providing terminal UI capabilities via [termbox2](https://github.com/termbox/termbox2).

## Installation

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:termbox2, path: "../termbox2_elixir"},
    {:termbox2_nif, "~> 0.1.9"}
  ]
end
```

Or, if published to Hex.pm:

```elixir
def deps do
  [
    {:termbox2, "~> 0.1.0"},
    {:termbox2_nif, "~> 0.1.9"}
  ]
end
```

## Usage

### Basic Example

```elixir
iex> Termbox2.init!()
:ok
iex> Termbox2.width!()
80
iex> Termbox2.height!()
24
iex> Termbox2.set_cell!(0, 0, ?A, [:red, :bold], :default)
:ok
iex> Termbox2.print!(1, 1, "Hello, world!")
:ok
iex> Termbox2.present!()
:ok
iex> Termbox2.shutdown!()
:ok
```

### Ergonomic API Features

- **Color/attribute atoms and lists:** Use `:red`, `:bold`, or `[:yellow, :bold]` for type safety and clarity.
- **Ergonomic helpers:** `set_cell/3,4,5`, `print/3,5`, and their `!` variants for easy usage.
- **Drawing utilities:** `draw_box/7`, `fill_rect/7`, `draw_row/5`, `draw_col/5`, `print_centered/4` for rapid TUI building.
- **Event helpers:** Pattern match on event types, keys, and modifiers with helpers like `Termbox2.Event`, `match_key`, `match_resize`, and `match_mouse`.
- **Property-based and edge-case testing:** Robust test suite covers random and edge-case inputs for all helpers and utilities.

### Drawing Helpers

```elixir
Termbox2.draw_box(0, 0, 10, 5, ?#, :cyan, :default)
Termbox2.fill_rect(1, 1, 9, 4, ?., :yellow, :default)
Termbox2.draw_row(2, 2, 'hello', :magenta, :default)
Termbox2.draw_col(5, 2, 'world', :blue, :default)
```

### Event Pattern Matching

```elixir
case Termbox2.poll_event!() do
  match_key(:esc) -> IO.puts("ESC pressed!")
  match_resize() -> IO.puts("Terminal resized!")
  %Termbox2.Event{type: :mouse, x: x, y: y} -> IO.puts("Mouse at #{x},#{y}")
end
```

### Color/Attr Introspection

```elixir
Termbox2.colors() #=> [:default, :black, :red, ...]
Termbox2.attrs()  #=> [:bold, :underline, :reverse]
```

### Error/Warning/Info Presets

```elixir
Termbox2.print_centered(10, "ERROR!", Termbox2.error_fg(), Termbox2.error_bg())
```

## Troubleshooting

- If you see `{:error, :failed}` or NIF loading errors, ensure you have built the NIF and your system has a compatible C toolchain.
- On macOS, you may need to run `xcode-select --install`.
- See [termbox2_nif documentation](https://hexdocs.pm/termbox2_nif) for low-level details.

## Testing and NIF Loading

When using the Elixir wrapper as part of this project, the NIF (`termbox2_nif`) is automatically found and loaded as long as you build via the Makefile or with Mix in the `wrappers/elixir` directory.

- No manual copying or symlinking of BEAM or NIF files is required for Elixir tests or development.
- The Makefile provides a `build-all` target to build and test all wrappers (Erlang, Elixir, Gleam) and ensure the NIF is available everywhere:

```sh
make build-all
```

This will compile everything and run all tests, ensuring the NIF is always in the correct place for Elixir and other wrappers.

If you encounter NIF loading errors, ensure you have built the project from the root or run `mix test` in this directory after compiling the NIF.

## API Documentation

Comprehensive module and function documentation is available via [ExDoc](https://hexdocs.pm/ex_doc/):

- Run `mix docs` and open `doc/index.html` for a browsable API reference.
- All public functions and structs are documented with typespecs and examples.
- See module-level docs in `Termbox2` and `Termbox2.Event` for quickstart, usage, and pattern matching tips.

## Advanced Usage

- Use `Termbox2.stream_events/0` for event-driven UIs.
- Use macros like `with_termbox` for safe resource management.
- Use property-based testing with StreamData for robust apps.

## License

MIT
