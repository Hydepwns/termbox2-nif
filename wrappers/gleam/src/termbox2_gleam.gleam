import gleam/int
import gleam/list
import gleam/result
import gleam/string

// FFI bindings to the termbox2_nif Erlang NIF
@external(erlang, "termbox2_nif", "tb_init")
fn tb_init() -> Int

@external(erlang, "termbox2_nif", "tb_shutdown")
fn tb_shutdown() -> Int

@external(erlang, "termbox2_nif", "tb_width")
fn tb_width() -> Int

@external(erlang, "termbox2_nif", "tb_height")
fn tb_height() -> Int

@external(erlang, "termbox2_nif", "tb_clear")
fn tb_clear() -> Int

@external(erlang, "termbox2_nif", "tb_present")
fn tb_present() -> Int

@external(erlang, "termbox2_nif", "tb_set_cursor")
fn tb_set_cursor(x: Int, y: Int) -> Int

@external(erlang, "termbox2_nif", "tb_hide_cursor")
fn tb_hide_cursor() -> Int

@external(erlang, "termbox2_nif", "tb_set_cell")
fn tb_set_cell(x: Int, y: Int, ch: Int, fg: Int, bg: Int) -> Int

// Advanced NIF FFI bindings
@external(erlang, "termbox2_nif", "tb_peek_event")
fn tb_peek_event(timeout_ms: Int) -> #(Int, Int, Int, Int)

@external(erlang, "termbox2_nif", "tb_poll_event")
fn tb_poll_event() -> #(Int, Int, Int, Int)

@external(erlang, "termbox2_nif", "tb_print")
fn tb_print(x: Int, y: Int, fg: Int, bg: Int, str: String) -> Int

@external(erlang, "termbox2_nif", "tb_set_input_mode")
fn tb_set_input_mode(mode: Int) -> Int

@external(erlang, "termbox2_nif", "tb_set_output_mode")
fn tb_set_output_mode(mode: Int) -> Int

@external(erlang, "termbox2_nif", "tb_set_clear_attrs")
fn tb_set_clear_attrs(fg: Int, bg: Int) -> Int

/// Represents a termbox2 event.
pub type Event {
  Event(type_: Int, mod: Int, key_or_char: Int, ch: Int)
}

/// Initializes the termbox2 library. Must be called before any other functions.
pub fn init() -> Result(Nil, String) {
  case tb_init() {
    0 -> Ok(Nil)
    _ -> Error("Failed to initialize termbox2")
  }
}

/// Finalizes the termbox2 library. Call when done with all termbox2 functions.
pub fn shutdown() -> Result(Nil, String) {
  case tb_shutdown() {
    0 -> Ok(Nil)
    _ -> Error("Failed to shutdown termbox2")
  }
}

/// Returns the width (columns) of the terminal window.
pub fn width() -> Result(Int, String) {
  let w = tb_width()
  case w {
    w if w >= 0 -> Ok(w)
    _ -> Error("Failed to get width")
  }
}

/// Returns the height (rows) of the terminal window.
pub fn height() -> Result(Int, String) {
  let h = tb_height()
  case h {
    h if h >= 0 -> Ok(h)
    _ -> Error("Failed to get height")
  }
}

/// Clears the internal back buffer using the default color.
pub fn clear() -> Result(Nil, String) {
  case tb_clear() {
    0 -> Ok(Nil)
    _ -> Error("Failed to clear termbox2 buffer")
  }
}

/// Synchronizes the internal back buffer with the terminal.
pub fn present() -> Result(Nil, String) {
  case tb_present() {
    0 -> Ok(Nil)
    _ -> Error("Failed to present termbox2 buffer")
  }
}

/// Sets the position of the cursor. (0, 0) is the upper-left.
pub fn set_cursor(x: Int, y: Int) -> Result(Nil, String) {
  case tb_set_cursor(x, y) {
    0 -> Ok(Nil)
    _ -> Error("Failed to set cursor")
  }
}

/// Hides the cursor (not supported by all terminals).
pub fn hide_cursor() -> Result(Nil, String) {
  case tb_hide_cursor() {
    0 -> Ok(Nil)
    _ -> Error("Failed to hide cursor")
  }
}

/// Sets the cell at (x, y) to character `ch` with foreground `fg` and background `bg`.
pub fn set_cell(
  x: Int,
  y: Int,
  ch: Int,
  fg: Int,
  bg: Int,
) -> Result(Nil, String) {
  case tb_set_cell(x, y, ch, fg, bg) {
    0 -> Ok(Nil)
    _ -> Error("Failed to set cell")
  }
}

/// Waits for an event up to `timeout_ms` milliseconds.
pub fn peek_event(timeout_ms: Int) -> Result(Event, String) {
  let result = tb_peek_event(timeout_ms)
  case result {
    #(type_, mod, key, ch) -> Ok(Event(type_, mod, key, ch))
  }
}

/// Waits for an event (no timeout).
pub fn poll_event() -> Result(Event, String) {
  let result = tb_poll_event()
  case result {
    #(type_, mod, key, ch) -> Ok(Event(type_, mod, key, ch))
  }
}

/// Prints the string at (x, y) with foreground and background colors.
pub fn print(
  x: Int,
  y: Int,
  fg: Int,
  bg: Int,
  str: String,
) -> Result(Nil, String) {
  case tb_print(x, y, fg, bg, str) {
    0 -> Ok(Nil)
    _ -> Error("Failed to print")
  }
}

/// Sets the termbox input mode. Returns the previous mode or error.
pub fn set_input_mode(mode: Int) -> Result(Int, String) {
  let prev = tb_set_input_mode(mode)
  case prev {
    prev if prev >= 0 -> Ok(prev)
    _ -> Error("Failed to set input mode")
  }
}

/// Sets the termbox output mode. Returns the previous mode or error.
pub fn set_output_mode(mode: Int) -> Result(Int, String) {
  let prev = tb_set_output_mode(mode)
  case prev {
    prev if prev >= 0 -> Ok(prev)
    _ -> Error("Failed to set output mode")
  }
}

/// Sets the default foreground and background attributes used by clear().
pub fn set_clear_attrs(fg: Int, bg: Int) -> Result(Nil, String) {
  case tb_set_clear_attrs(fg, bg) {
    0 -> Ok(Nil)
    _ -> Error("Failed to set clear attrs")
  }
}

// Color and attribute helpers
pub fn color_to_int(color: String) -> Int {
  case color {
    "default" -> 0
    "black" -> 1
    "red" -> 2
    "green" -> 3
    "yellow" -> 4
    "blue" -> 5
    "magenta" -> 6
    "cyan" -> 7
    "white" -> 8
    _ -> 0
  }
}

pub fn attr_to_int(attr: String) -> Int {
  case attr {
    "bold" -> 0x0100
    "underline" -> 0x0200
    "reverse" -> 0x0400
    _ -> 0
  }
}

/// Converts a list of color/attribute names to a single integer value.
pub fn color_attr_to_int(attrs: List(String)) -> Int {
  list.fold(attrs, 0, fn(acc, a) { acc + color_to_int(a) + attr_to_int(a) })
}

// Ergonomic event helpers
pub type EventType {
  Key
  Resize
  Mouse
  Unknown(Int)
}

pub fn event_type_from_int(i: Int) -> EventType {
  case i {
    0 -> Key
    1 -> Resize
    2 -> Mouse
    _ -> Unknown(i)
  }
}

pub fn event_mods_from_int(i: Int) -> List(String) {
  let mods = []
  let mods = case int.bitwise_and(i, 1) != 0 {
    True -> list.append(["alt"], mods)
    False -> mods
  }
  let mods = case int.bitwise_and(i, 2) != 0 {
    True -> list.append(["ctrl"], mods)
    False -> mods
  }
  let mods = case int.bitwise_and(i, 4) != 0 {
    True -> list.append(["shift"], mods)
    False -> mods
  }
  let mods = case int.bitwise_and(i, 8) != 0 {
    True -> list.append(["meta"], mods)
    False -> mods
  }
  let mods = case int.bitwise_and(i, 16) != 0 {
    True -> list.append(["super"], mods)
    False -> mods
  }
  let mods = case int.bitwise_and(i, 32) != 0 {
    True -> list.append(["hyper"], mods)
    False -> mods
  }
  mods
}

/// Decodes a termbox2 Event into a more ergonomic record.
pub type DecodedEvent {
  DecodedEvent(type_: EventType, mods: List(String), key: Int, ch: Int)
}

pub fn decode_event(e: Event) -> DecodedEvent {
  let Event(type_, mod, key, ch) = e
  DecodedEvent(event_type_from_int(type_), event_mods_from_int(mod), key, ch)
}

// Ergonomic color and attribute enums
pub type Color {
  Default
  Black
  Red
  Green
  Yellow
  Blue
  Magenta
  Cyan
  White
}

pub type Attr {
  Bold
  Underline
  Reverse
}

/// Converts a Color enum to its integer value.
pub fn color_to_int_enum(color: Color) -> Int {
  case color {
    Default -> 0
    Black -> 1
    Red -> 2
    Green -> 3
    Yellow -> 4
    Blue -> 5
    Magenta -> 6
    Cyan -> 7
    White -> 8
  }
}

/// Converts an Attr enum to its integer value.
pub fn attr_to_int_enum(attr: Attr) -> Int {
  case attr {
    Bold -> 0x0100
    Underline -> 0x0200
    Reverse -> 0x0400
  }
}

/// Converts lists of Color and Attr enums to a single integer value.
pub fn color_attrs_to_int(colors: List(Color), attrs: List(Attr)) -> Int {
  let color_val =
    list.fold(colors, 0, fn(acc, c) { acc + color_to_int_enum(c) })
  let attr_val = list.fold(attrs, 0, fn(acc, a) { acc + attr_to_int_enum(a) })
  color_val + attr_val
}

/// Ergonomic set_cell: accepts Color enums for fg/bg.
pub fn set_cell_friendly(
  x: Int,
  y: Int,
  ch: Int,
  fg: Color,
  bg: Color,
) -> Result(Nil, String) {
  set_cell(x, y, ch, color_to_int_enum(fg), color_to_int_enum(bg))
}

/// Ergonomic set_cell: accepts lists of Color/Attr enums for fg/bg.
pub fn set_cell_with_attrs(
  x: Int,
  y: Int,
  ch: Int,
  fg: List(Color),
  fg_attrs: List(Attr),
  bg: List(Color),
  bg_attrs: List(Attr),
) -> Result(Nil, String) {
  set_cell(
    x,
    y,
    ch,
    color_attrs_to_int(fg, fg_attrs),
    color_attrs_to_int(bg, bg_attrs),
  )
}

/// Ergonomic print: accepts Color enums for fg/bg.
pub fn print_friendly(
  x: Int,
  y: Int,
  fg: Color,
  bg: Color,
  str: String,
) -> Result(Nil, String) {
  print(x, y, color_to_int_enum(fg), color_to_int_enum(bg), str)
}

/// Ergonomic print: accepts lists of Color/Attr enums for fg/bg.
pub fn print_with_attrs(
  x: Int,
  y: Int,
  fg: List(Color),
  fg_attrs: List(Attr),
  bg: List(Color),
  bg_attrs: List(Attr),
  str: String,
) -> Result(Nil, String) {
  print(
    x,
    y,
    color_attrs_to_int(fg, fg_attrs),
    color_attrs_to_int(bg, bg_attrs),
    str,
  )
}

/// Prints a string centered on the given row.
pub fn print_centered(
  y: Int,
  str: String,
  fg: Color,
  bg: Color,
) -> Result(Nil, String) {
  let width_val = width() |> result.unwrap(0)
  let x =
    int.max(
      int.divide(width_val - string.length(str), 2) |> result.unwrap(0),
      0,
    )
  print_friendly(x, y, fg, bg, str)
}

// Draws a rectangular box outline from (x1, y1) to (x2, y2) using ch, fg, bg
pub fn draw_box(
  x1: Int,
  y1: Int,
  x2: Int,
  y2: Int,
  ch: Int,
  fg: Color,
  bg: Color,
) -> Result(Nil, String) {
  let min_x = int.min(x1, x2)
  let max_x = int.max(x1, x2)
  let min_y = int.min(y1, y2)
  let max_y = int.max(y1, y2)
  // Top and bottom
  let top =
    list.range(min_x, max_x)
    |> list.map(fn(x) { set_cell_friendly(x, min_y, ch, fg, bg) })
  let bottom =
    list.range(min_x, max_x)
    |> list.map(fn(x) { set_cell_friendly(x, max_y, ch, fg, bg) })
  // Left and right
  let left =
    list.range(min_y + 1, max_y - 1)
    |> list.map(fn(y) { set_cell_friendly(min_x, y, ch, fg, bg) })
  let right =
    list.range(min_y + 1, max_y - 1)
    |> list.map(fn(y) { set_cell_friendly(max_x, y, ch, fg, bg) })
  let results = list.append(top, list.append(bottom, list.append(left, right)))
  results
  |> list.fold(Ok(Nil), fn(acc, r) {
    case acc {
      Error(e) -> Error(e)
      Ok(_) -> r
    }
  })
}

// Fills a rectangle from (x1, y1) to (x2, y2) with ch, fg, bg
pub fn fill_rect(
  x1: Int,
  y1: Int,
  x2: Int,
  y2: Int,
  ch: Int,
  fg: Color,
  bg: Color,
) -> Result(Nil, String) {
  let min_x = int.min(x1, x2)
  let max_x = int.max(x1, x2)
  let min_y = int.min(y1, y2)
  let max_y = int.max(y1, y2)
  let results =
    list.range(min_y, max_y)
    |> list.flat_map(fn(y) {
      list.range(min_x, max_x)
      |> list.map(fn(x) { set_cell_friendly(x, y, ch, fg, bg) })
    })
  results
  |> list.fold(Ok(Nil), fn(acc, r) {
    case acc {
      Error(e) -> Error(e)
      Ok(_) -> r
    }
  })
}

// Key type and decode_key implementation
pub type Key {
  Esc
  Enter
  F(Int)
  Char(Int)
  UnknownKey(Int)
}

pub fn decode_key(code: Int) -> Key {
  case code {
    0x1B -> Esc
    0x0D -> Enter
    0x70 -> F(1)
    c if c >= 32 && c <= 126 -> Char(c)
    other -> UnknownKey(other)
  }
}
