defmodule Termbox2 do
  require Bitwise

  @moduledoc """
  Elixir wrapper for the `:termbox2_nif` NIF library.

  This module provides idiomatic Elixir functions for building terminal UIs using [termbox2](https://github.com/termbox/termbox2).

  ## Features
  - Simple, ergonomic API for terminal input/output
  - Color and attribute helpers (atoms, lists, or integers)
  - Event handling (keyboard, resize, mouse)
  - Pattern matching and streaming support for events
  - Macros and helpers for drawing and color introspection

  ## Quickstart
  ```elixir
  # Start termbox2
  :ok = Termbox2.init()
  :ok = Termbox2.clear()
  :ok = Termbox2.set_cell(0, 0, ?A, :red, :default)
  :ok = Termbox2.present()
  :ok = Termbox2.shutdown()
  ```

  See the README for installation, advanced usage, and troubleshooting.

  See also: `Termbox2.Event` for event struct and pattern matching.
  """

  @typedoc "Termbox2 input modes."
  @type input_mode :: :esc | :alt | :normal | integer()

  @typedoc "Termbox2 output modes."
  @type output_mode :: :normal | :"256" | :"216" | :grayscale | integer()

  @typedoc "Result returned by most Termbox2 functions."
  @type result :: :ok | {:error, atom()}

  @typedoc "Color or attribute."
  @type color_attr ::
    :default | :black | :red | :green | :yellow | :blue | :magenta | :cyan | :white |
    :bold | :underline | :reverse | integer() | [atom()]

  # Color/attribute mapping (termbox2 constants)
  @color_map %{
    default: 0,
    black: 1,
    red: 2,
    green: 3,
    yellow: 4,
    blue: 5,
    magenta: 6,
    cyan: 7,
    white: 8
  }
  @attr_map %{
    bold: 0x0100,
    underline: 0x0200,
    reverse: 0x0400
  }

  # Default fg/bg for convenience
  @default_fg :default
  @default_bg :default

  @doc """
  Initializes the termbox library. Must be called before any other functions.
  Returns :ok or {:error, reason}.
  """
  @spec init() :: result
  def init do
    case nif_module().tb_init() do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Finalizes the termbox library. Call when done with all termbox functions.
  Returns :ok or {:error, reason}.
  """
  @spec shutdown() :: result
  def shutdown do
    case nif_module().tb_shutdown() do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Returns the width (columns) of the terminal window.
  Returns {:ok, width} or {:error, reason}.
  """
  @spec width() :: {:ok, non_neg_integer()} | {:error, atom()}
  def width do
    case nif_module().tb_width() do
      w when is_integer(w) and w >= 0 -> {:ok, w}
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Returns the height (rows) of the terminal window.
  Returns {:ok, height} or {:error, reason}.
  """
  @spec height() :: {:ok, non_neg_integer()} | {:error, atom()}
  def height do
    case nif_module().tb_height() do
      h when is_integer(h) and h >= 0 -> {:ok, h}
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Clears the internal back buffer using the default color.
  Returns :ok or {:error, reason}.
  """
  @spec clear() :: result
  def clear do
    case nif_module().tb_clear() do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Synchronizes the internal back buffer with the terminal.
  Returns :ok or {:error, reason}.
  """
  @spec present() :: result
  def present do
    case nif_module().tb_present() do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Sets the position of the cursor. (0, 0) is the upper-left.
  Returns :ok or {:error, reason}.
  """
  @spec set_cursor(non_neg_integer(), non_neg_integer()) :: result
  def set_cursor(x, y) when is_integer(x) and is_integer(y) and x >= 0 and y >= 0 do
    case nif_module().tb_set_cursor(x, y) do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end
  def set_cursor(_, _), do: {:error, :badarg}

  @doc """
  Hides the cursor (not supported by all terminals).
  Returns :ok or {:error, reason}.
  """
  @spec hide_cursor() :: result
  def hide_cursor do
    case nif_module().tb_hide_cursor() do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Sets the cell at (x, y) to character `ch` with default fg/bg (:default).
  """
  @spec set_cell(non_neg_integer(), non_neg_integer(), integer() | char | binary) :: result
  def set_cell(x, y, ch), do: set_cell(x, y, ch, @default_fg, @default_bg)

  @doc """
  Sets the cell at (x, y) to character `ch` with foreground `fg` and background `bg`.
  `fg` and `bg` can be color/attribute atoms, lists, or integers.
  Returns :ok or {:error, reason}.
  """
  @spec set_cell(non_neg_integer(), non_neg_integer(), integer() | char | binary, color_attr, color_attr) :: result
  def set_cell(x, y, ch, fg, bg)
      when is_integer(x) and is_integer(y) and x >= 0 and y >= 0 and
           (is_integer(ch) or is_binary(ch)) do
    ch_val =
      cond do
        is_integer(ch) -> ch
        is_binary(ch) and byte_size(ch) == 1 -> :binary.decode_unsigned(ch)
        is_binary(ch) and byte_size(ch) > 0 ->
          case :unicode.characters_to_list(ch) do
            [codepoint | _] when is_integer(codepoint) -> codepoint
            _ -> nil
          end
        true -> nil
      end
    fg_val = color_attr_to_int(fg)
    bg_val = color_attr_to_int(bg)
    if is_integer(ch_val) do
      case nif_module().tb_set_cell(x, y, ch_val, fg_val, bg_val) do
        0 -> :ok
        -1 -> {:error, :failed}
        other -> {:error, other}
      end
    else
      {:error, :badarg}
    end
  end
  def set_cell(_, _, _, _, _), do: {:error, :badarg}

  defmodule Event do
    @moduledoc """
    Struct representing a termbox2 event.

    ## Fields
      * `:type` - event type (`:key`, `:resize`, `:mouse`, or integer)
      * `:mod` - modifier keys (list of atoms or integer)
      * `:key` - key atom or integer (for key events)
      * `:ch` - character code (for key events)
      * `:w`, `:h` - width/height (for resize events)
      * `:x`, `:y` - coordinates (for mouse events)

    ## Example: Pattern Matching
    ```elixir
    case Termbox2.poll_event() do
      {:ok, %Termbox2.Event{type: :key, key: :esc}} ->
        IO.puts("Escape pressed!")
      {:ok, %Termbox2.Event{type: :resize, w: w, h: h}} ->
        IO.puts("Resized to \", \", w, "x", h)
      _ -> :ignore
    end
    ```
    """
    defstruct type: nil, mod: [], key: nil, ch: nil, w: nil, h: nil, x: nil, y: nil

    @type t :: %__MODULE__{
      type: :key | :resize | :mouse | integer(),
      mod: [atom()] | integer(),
      key: atom() | integer() | nil,
      ch: integer() | nil,
      w: integer() | nil,
      h: integer() | nil,
      x: integer() | nil,
      y: integer() | nil
    }
  end

  # Expanded event type/mod/key mapping
  @event_type_map %{0 => :key, 1 => :resize, 2 => :mouse, 3 => :unknown}
  @event_mod_map %{
    0 => [],
    1 => [:alt],
    2 => [:ctrl],
    4 => [:shift],
    8 => [:meta]
  }
  @event_key_map %{
    0x1B => :esc, 0x0D => :enter, 0x09 => :tab, 0x7F => :backspace,
    0x20 => :space, 0x08 => :bs, 0x0A => :newline,
    0x70 => :f1, 0x71 => :f2, 0x72 => :f3, 0x73 => :f4, 0x74 => :f5,
    0x75 => :f6, 0x76 => :f7, 0x77 => :f8, 0x78 => :f9, 0x79 => :f10,
    0x7A => :f11, 0x7B => :f12
    # ... add more as needed ...
  }

  defp int_to_event_type(i), do: Map.get(@event_type_map, i, i)
  defp int_to_event_mod(i) when is_integer(i) do
    Enum.flat_map(@event_mod_map, fn {bit, mod} -> if (Bitwise.&&&(i, bit)) != 0, do: mod, else: [] end)
  end
  defp int_to_event_key(i), do: Map.get(@event_key_map, i, i)

  @doc """
  Waits for an event up to `timeout_ms` milliseconds, returning {:ok, %Termbox2.Event{}} or {:error, reason}.
  """
  @spec peek_event(non_neg_integer()) :: {:ok, Termbox2.Event.t()} | {:error, atom()}
  def peek_event(timeout_ms) when is_integer(timeout_ms) and timeout_ms >= 0 do
    case nif_module().tb_peek_event(timeout_ms) do
      {:ok, type, mod, key_or_char} ->
        {:ok, %Termbox2.Event{
          type: int_to_event_type(type),
          mod: int_to_event_mod(mod),
          key: int_to_event_key(key_or_char),
          ch: key_or_char
        }}
      -1 -> {:error, :timeout}
      other -> {:error, other}
    end
  end
  def peek_event(_), do: {:error, :badarg}

  @doc """
  Waits for an event (no timeout), returning {:ok, %Termbox2.Event{}} or {:error, reason}.
  """
  @spec poll_event() :: {:ok, Termbox2.Event.t()} | {:error, atom()}
  def poll_event do
    case nif_module().tb_poll_event() do
      {:ok, type, mod, key_or_char} ->
        {:ok, %Termbox2.Event{
          type: int_to_event_type(type),
          mod: int_to_event_mod(mod),
          key: int_to_event_key(key_or_char),
          ch: key_or_char
        }}
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  @doc """
  Prints the string at (x, y) with default fg/bg (:default).
  """
  @spec print(non_neg_integer(), non_neg_integer(), binary) :: result
  def print(x, y, str), do: print(x, y, @default_fg, @default_bg, str)

  @doc """
  Prints the string at (x, y) with foreground and background colors.
  `fg` and `bg` can be color/attribute atoms, lists, or integers.
  Returns :ok or {:error, reason}.
  """
  @spec print(non_neg_integer(), non_neg_integer(), color_attr, color_attr, binary) :: result
  def print(x, y, fg, bg, str)
      when is_integer(x) and is_integer(y) and x >= 0 and y >= 0 and
           is_binary(str) do
    fg_val = color_attr_to_int(fg)
    bg_val = color_attr_to_int(bg)
    case nif_module().tb_print(x, y, fg_val, bg_val, str) do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end
  def print(_, _, _, _, _), do: {:error, :badarg}

  @doc """
  Sets the termbox input mode. Accepts :esc, :alt, :normal, or an integer.
  Returns the previous mode as an atom or integer, or {:error, :badarg}.
  """
  @spec set_input_mode(input_mode) :: input_mode | {:error, :badarg}
  def set_input_mode(mode) when is_atom(mode) or is_integer(mode) do
    mode_int = input_mode_to_int(mode)
    if is_integer(mode_int) do
      prev = nif_module().tb_set_input_mode(mode_int)
      int_to_input_mode(prev)
    else
      {:error, :badarg}
    end
  end

  defp input_mode_to_int(:esc), do: 0
  defp input_mode_to_int(:alt), do: 1
  defp input_mode_to_int(:normal), do: 2
  defp input_mode_to_int(i) when is_integer(i), do: i
  defp input_mode_to_int(_), do: nil

  defp int_to_input_mode(0), do: :esc
  defp int_to_input_mode(1), do: :alt
  defp int_to_input_mode(2), do: :normal
  defp int_to_input_mode(i) when is_integer(i), do: i
  defp int_to_input_mode(_), do: {:error, :badarg}

  @doc """
  Sets the termbox output mode. Accepts :normal, :256, :216, :grayscale, or an integer.
  Returns the previous mode as an atom or integer, or {:error, :badarg}.
  """
  @spec set_output_mode(output_mode) :: output_mode | {:error, :badarg}
  def set_output_mode(mode) when is_atom(mode) or is_integer(mode) do
    mode_int = output_mode_to_int(mode)
    if is_integer(mode_int) do
      prev = nif_module().tb_set_output_mode(mode_int)
      int_to_output_mode(prev)
    else
      {:error, :badarg}
    end
  end

  defp output_mode_to_int(:normal), do: 0
  defp output_mode_to_int(:"256"), do: 1
  defp output_mode_to_int(:"216"), do: 2
  defp output_mode_to_int(:grayscale), do: 3
  defp output_mode_to_int(i) when is_integer(i), do: i
  defp output_mode_to_int(_), do: nil

  defp int_to_output_mode(0), do: :normal
  defp int_to_output_mode(1), do: :"256"
  defp int_to_output_mode(2), do: :"216"
  defp int_to_output_mode(3), do: :grayscale
  defp int_to_output_mode(i) when is_integer(i), do: i
  defp int_to_output_mode(_), do: {:error, :badarg}

  @doc """
  Sets the default foreground and background attributes used by `clear/0`.
  `fg` and `bg` can be color/attribute atoms, lists, or integers.
  Returns :ok or {:error, reason}.
  """
  @spec set_clear_attrs(color_attr, color_attr) :: result
  def set_clear_attrs(fg, bg) do
    fg_val = color_attr_to_int(fg)
    bg_val = color_attr_to_int(bg)
    case nif_module().tb_set_clear_attrs(fg_val, bg_val) do
      0 -> :ok
      -1 -> {:error, :failed}
      other -> {:error, other}
    end
  end

  defp color_attr_to_int(val) when is_integer(val), do: val
  defp color_attr_to_int(atom) when is_atom(atom) do
    Map.get(@color_map, atom) || Map.get(@attr_map, atom) || 0
  end
  defp color_attr_to_int(list) when is_list(list) do
    Enum.reduce(list, 0, fn item, acc -> Bitwise.|||(acc, color_attr_to_int(item)) end)
  end
  defp color_attr_to_int(_), do: 0

  @doc """
  Hello world.

  ## Examples

      iex> Termbox2.hello()
      :world

  """
  def hello do
    :world
  end

  @doc """
  Returns the integer value for a color/attribute atom or list.
  """
  @spec color_value(color_attr) :: integer()
  def color_value(attr), do: color_attr_to_int(attr)

  @doc """
  Returns the integer value for a color atom (no attributes).
  """
  @spec color(color_attr) :: integer()
  def color(attr) when is_atom(attr), do: Map.get(@color_map, attr, 0)
  def color(_), do: 0

  @doc """
  Returns the integer value for an attribute atom (no color).
  """
  @spec attr(color_attr) :: integer()
  def attr(attr) when is_atom(attr), do: Map.get(@attr_map, attr, 0)
  def attr(_), do: 0

  @doc """
  Returns a Stream of events (infinite), for use with Enum/Stream.
  Example: Enum.take(Termbox2.stream_events(), 10)
  """
  @spec stream_events() :: Enumerable.t()
  def stream_events do
    Stream.resource(
      fn -> :ok end,
      fn _ ->
        case poll_event() do
          {:ok, event} -> {[event], :ok}
          {:error, _} -> {:halt, :ok}
        end
      end,
      fn _ -> :ok end
    )
  end

  defmacro __using__(_opts) do
    quote do
      import Termbox2, only: [with_termbox: 1, match_key: 1, match_resize: 0, match_mouse: 0]
    end
  end

  for {fun, arity} <- [
    {:init, 0}, {:shutdown, 0}, {:clear, 0}, {:present, 0},
    {:set_cursor, 2}, {:hide_cursor, 0}, {:set_cell, 3}, {:set_cell, 5},
    {:print, 3}, {:print, 5}, {:set_clear_attrs, 2}
  ] do
    bang_fun = String.to_atom(Atom.to_string(fun) <> "!")
    args = Macro.generate_arguments(arity, __MODULE__)
    def unquote(bang_fun)(unquote_splicing(args)) do
      case apply(__MODULE__, unquote(fun), [unquote_splicing(args)]) do
        :ok -> :ok
        {:ok, v} -> v
        {:error, reason} -> raise "Termbox2 #{unquote(fun)} failed: #{inspect(reason)}"
        other -> other
      end
    end
  end

  @doc """
  Draws a box from (x1, y1) to (x2, y2) with a given character and color.
  """
  @spec draw_box(integer, integer, integer, integer, integer | char | binary, color_attr, color_attr) :: :ok
  def draw_box(x1, y1, x2, y2, ch, fg \\ :default, bg \\ :default) do
    for x <- x1..x2, y <- [y1, y2], do: set_cell(x, y, ch, fg, bg)
    for y <- y1..y2, x <- [x1, x2], do: set_cell(x, y, ch, fg, bg)
    :ok
  end

  @doc """
  Fills a rectangle from (x1, y1) to (x2, y2) with a given character and color.
  """
  @spec fill_rect(integer, integer, integer, integer, integer | char | binary, color_attr, color_attr) :: :ok
  def fill_rect(x1, y1, x2, y2, ch, fg \\ :default, bg \\ :default) do
    for x <- x1..x2, y <- y1..y2, do: set_cell(x, y, ch, fg, bg)
    :ok
  end

  @doc """
  Moves the cursor by dx, dy relative to its current position.
  """
  @spec move_cursor_by(integer, integer) :: result
  def move_cursor_by(dx, dy) do
    {:ok, w} = width()
    {:ok, h} = height()
    set_cursor(w + dx, h + dy)
  end

  @doc """
  Clears the screen and presents the buffer.
  """
  @spec clear_screen() :: result
  def clear_screen do
    clear()
    present()
  end

  @doc """
  Macro to match a key event.
  """
  defmacro match_key(key), do: quote(do: %Termbox2.Event{type: :key, key: unquote(key)})
  @doc """
  Macro to match a resize event.
  """
  defmacro match_resize, do: quote(do: %Termbox2.Event{type: :resize})
  @doc """
  Macro to match a mouse event.
  """
  defmacro match_mouse, do: quote(do: %Termbox2.Event{type: :mouse})

  @doc "Returns a preset for error messages."
  def error_fg, do: [:red, :bold]
  def error_bg, do: :default
  @doc "Returns a preset for warning messages."
  def warning_fg, do: [:yellow, :bold]
  def warning_bg, do: :default
  @doc "Returns a preset for info messages."
  def info_fg, do: [:cyan]
  def info_bg, do: :default

  @doc """
  Prints a string centered on the given row.
  """
  @spec print_centered(integer, binary, color_attr, color_attr) :: result
  def print_centered(y, str, fg \\ :default, bg \\ :default) do
    {:ok, w} = width()
    x = max(div(w - String.length(str), 2), 0)
    print(x, y, fg, bg, str)
  end

  @doc """
  Macro to ensure proper init/shutdown of termbox2.
  """
  defmacro with_termbox(do: block) do
    quote do
      Termbox2.init()
      try do
        unquote(block)
      after
        Termbox2.shutdown()
      end
    end
  end

  @doc """
  Draws a list of characters horizontally starting at (x, y).
  """
  @spec draw_row(integer, integer, [integer | char | binary], color_attr, color_attr) :: :ok
  def draw_row(x, y, chars, fg \\ :default, bg \\ :default) do
    Enum.with_index(chars)
    |> Enum.each(fn {ch, i} -> set_cell(x + i, y, ch, fg, bg) end)
    :ok
  end

  @doc """
  Draws a list of characters vertically starting at (x, y).
  """
  @spec draw_col(integer, integer, [integer | char | binary], color_attr, color_attr) :: :ok
  def draw_col(x, y, chars, fg \\ :default, bg \\ :default) do
    Enum.with_index(chars)
    |> Enum.each(fn {ch, i} -> set_cell(x, y + i, ch, fg, bg) end)
    :ok
  end

  @doc "Returns all supported color atoms."
  def colors, do: Map.keys(@color_map)
  @doc "Returns all supported attribute atoms."
  def attrs, do: Map.keys(@attr_map)

  @doc false
  defp nif_module do
    Application.get_env(:termbox2_elixir, :nif_module, :termbox2_nif)
  end
end

defmodule Termbox2.NIFBehaviour do
  @callback tb_init() :: integer()
  @callback tb_shutdown() :: integer()
  @callback tb_width() :: integer()
  @callback tb_height() :: integer()
  @callback tb_clear() :: integer()
  @callback tb_present() :: integer()
  @callback tb_set_cursor(integer(), integer()) :: integer()
  @callback tb_hide_cursor() :: integer()
  @callback tb_set_cell(integer(), integer(), integer(), integer(), integer()) :: integer()
  @callback tb_peek_event(integer()) :: tuple() | integer()
  @callback tb_poll_event() :: tuple() | integer()
  @callback tb_print(integer(), integer(), integer(), integer(), binary()) :: integer()
  @callback tb_set_input_mode(integer()) :: integer()
  @callback tb_set_output_mode(integer()) :: integer()
  @callback tb_set_clear_attrs(integer(), integer()) :: integer()
end
