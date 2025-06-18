%% @author Wade Mealing <wmealing@gmail.com>
%% @version 0.1.5
%% @doc Compatibility module that forwards calls to termbox2_nif.
%% This exists to maintain backward compatibility with existing code.
%% New code should use termbox2_nif module directly.

-module(termbox2).

-export([
    tb_init/0,
    tb_shutdown/0,
    tb_width/0,
    tb_height/0,
    tb_clear/0,
    tb_present/0,
    tb_set_cursor/2,
    tb_hide_cursor/0,
    tb_set_cell/5,
    tb_peek_event/1,
    tb_poll_event/0,
    tb_print/5,
    tb_set_input_mode/1,
    tb_set_output_mode/1,
    tb_set_clear_attrs/2,
    tb_set_title/1,
    tb_set_position/2
]).

-type tb_event() :: {tb_event, integer(), integer(), integer(), integer()}.
-type tb_input_mode() :: integer().
-type tb_output_mode() :: integer().

%% @doc Initializes the termbox library. This function should be called before any other functions.
-spec tb_init() -> ok | {error, term()}.
tb_init() ->
    termbox2_nif:tb_init().

%% @doc The library must be finalized using the tb_shutdown() function.  Called only when no more tb_ functions are required.
%% @returns Nothing.
%% @end
-spec tb_shutdown() -> ok.
tb_shutdown() ->
    termbox2_nif:tb_shutdown().

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in columns
-spec tb_width() -> integer().
tb_width() ->
    termbox2_nif:tb_width().

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in rows
-spec tb_height() -> integer().
tb_height() ->
    termbox2_nif:tb_height().

%% @doc Clears the internal back buffer using TB_DEFAULT color.
-spec tb_clear() -> ok.
tb_clear() ->
    termbox2_nif:tb_clear().

%% @doc Synchronizes the internal back buffer with the terminal by writing to the tty.
-spec tb_present() -> ok.
tb_present() ->
    termbox2_nif:tb_present().

%% @doc Sets the position of the cursor. Upper-left character is (0, 0).
-spec tb_set_cursor(X :: integer(), Y :: integer()) -> ok.
tb_set_cursor(X, Y) ->
    termbox2_nif:tb_set_cursor(X, Y).

%% @doc Hides the cursor, not every tty supports this.
-spec tb_hide_cursor() -> ok.
tb_hide_cursor() ->
    termbox2_nif:tb_hide_cursor().

%% @doc Sets the individual position of the screen at x,y to character ch, with foreground color fg and bg color bg.
-spec tb_set_cell(X :: integer(), Y :: integer(), Ch :: integer(),
                 Fg :: integer(), Bg :: integer()) -> ok.
tb_set_cell(X, Y, Ch, Fg, Bg) ->
    termbox2_nif:tb_set_cell(X, Y, Ch, Fg, Bg).

%% @doc Wait for an event up to timeout_ms milliseconds, returning event details in a tuple.
%% @returns "A THING"
-spec tb_peek_event(Timeout :: integer()) -> {ok, tb_event()} | {error, term()}.
tb_peek_event(Timeout) ->
    termbox2_nif:tb_peek_event(Timeout).

%% @doc Same as tb_peek_event except no timeout, probably going to beat up erlang scheduler, maybe ?
-spec tb_poll_event() -> {ok, tb_event()} | {error, term()}.
tb_poll_event() ->
    termbox2_nif:tb_poll_event().

%% @doc Print the string at the X and y locations with fg and background colors set.
-spec tb_print(X :: integer(), Y :: integer(), Fg :: integer(),
              Bg :: integer(), Str :: string()) -> ok.
tb_print(X, Y, Fg, Bg, Str) ->
    termbox2_nif:tb_print(X, Y, Fg, Bg, Str).

%% @doc Sets the termbox input mode.
-spec tb_set_input_mode(Mode :: tb_input_mode()) -> tb_input_mode().
tb_set_input_mode(Mode) ->
    termbox2_nif:tb_set_input_mode(Mode).

%% @doc Sets the termbox output mode.
-spec tb_set_output_mode(Mode :: tb_output_mode()) -> tb_output_mode().
tb_set_output_mode(Mode) ->
    termbox2_nif:tb_set_output_mode(Mode).

%% @doc Sets the default foreground and background attributes used by tb_clear().
-spec tb_set_clear_attrs(Fg :: integer(), Bg :: integer()) -> ok.
tb_set_clear_attrs(Fg, Bg) ->
    termbox2_nif:tb_set_clear_attrs(Fg, Bg).

%% @doc Sets the title of the terminal window.
-spec tb_set_title(Title :: string()) -> ok.
tb_set_title(Title) ->
    termbox2_nif:tb_set_title(Title).

%% @doc Sets the position of the terminal window.
-spec tb_set_position(X :: integer(), Y :: integer()) -> ok.
tb_set_position(X, Y) ->
    termbox2_nif:tb_set_position(X, Y).
