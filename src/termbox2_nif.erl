%% @author DROO AMOR <drew@axol.io>
%% @version 0.2.0

%% @doc
%% Erlang NIF for termbox2. Provides low-level terminal UI functions for BEAM languages.

-module('termbox2_nif').


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
    tb_set_clear_attrs/2
]).
-nifs([
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
    tb_set_clear_attrs/2
]).
-on_load(init/0).

init() ->
    AppNames = [termbox2_gleam, termbox2_nif], % Check for both, as priv dir might be under either
    NifName = "termbox2_nif", % The actual NIF shared library name (without .so/.dylib)
    PrivPaths = lists:flatmap(
        fun(App) ->
            case code:priv_dir(App) of
                {error, _} -> [];
                Dir -> [filename:join(Dir, NifName)] % Path to NIF *file* itself
            end
        end,
        AppNames
    ),
    EnvPath = case os:getenv("TERMBOX2_NIF_PATH") of
        false -> [];
        Path -> [Path] % User-specified direct path to NIF file or dir
    end,
    BasePaths = PrivPaths ++ EnvPath, % Combine discovered and env paths
    Exts = ["", ".so", ".dylib"], % Common extensions to try with base paths

    % Create a list of full path attempts by appending extensions to base paths
    Paths = lists:flatmap(
        fun(Base) ->
            lists:map(fun(Ext) -> Base ++ Ext end, Exts)
        end,
        BasePaths
    ),

    io:format("[termbox2_nif] Trying NIF paths:~n", []),
    lists:foreach(
        fun(P) ->
            io:format("  Path: ~ts~n", [P])
        end,
        Paths
    ),
    try_load_nif(Paths).

try_load_nif([Path | Rest]) ->
    Result = erlang:load_nif(Path, 0),
    io:format("[termbox2_nif] load_nif(~ts) -> ~tp~n", [Path, Result]),
    case Result of
        ok -> ok;
        {error, {already_loaded, _}} -> ok; % NIF already loaded is also fine
        Error ->
            case Rest of
                [] -> Error; % No more paths to try, return the last error
                _  -> try_load_nif(Rest) % Try the next path
            end
    end;
try_load_nif([]) ->
    % This case should ideally not be reached if BasePaths was empty and Paths became empty.
    % If it is, it means no NIF paths were found or generated.
    io:format("[termbox2_nif] Error: No NIF paths were available to try.~n", []),
    {error, nif_path_not_found}.

%% @doc Initializes the termbox library. This function should be called before any other functions.
-spec tb_init() -> ok | {error, term()}.
tb_init() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc The library must be finalized using the tb_shutdown() function.  Called only when no more tb_ functions are required.
%% @returns Nothing.
%% @end
-spec tb_shutdown() -> ok.
tb_shutdown() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in columns
-spec tb_width() -> integer().
tb_width() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in rows
-spec tb_height() -> integer().
tb_height() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Clears the internal back buffer using TB_DEFAULT color.
-spec tb_clear() -> ok.
tb_clear() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Synchronizes the internal back buffer with the terminal by writing to the tty.
-spec tb_present() -> ok.
tb_present() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the position of the cursor. Upper-left character is (0, 0).
-spec tb_set_cursor(integer(), integer()) -> ok.
tb_set_cursor(_X, _Y) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Hides the cursor, not every tty supports this.
-spec tb_hide_cursor() -> ok.
tb_hide_cursor() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the individual position of the screen at x,y to character ch, with foreground color fg and bg color bg.
-spec tb_set_cell(integer(), integer(), integer(), integer(), integer()) -> ok.
tb_set_cell(_X, _Y, _Ch, _Fg, _Bg) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Wait for an event up to timeout_ms milliseconds, returning event details in a tuple.
%% @returns "A THING"
-spec tb_peek_event(integer()) -> {ok, tuple()} | {error, term()}.
tb_peek_event(_Timeout) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Same as tb_peek_event except no timeout, probably going to beat up erlang scheduler, maybe ?
-spec tb_poll_event() -> {ok, tuple()} | {error, term()}.
tb_poll_event() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Print the string at the X and y locations with fg and background colors set.
-spec tb_print(integer(), integer(), integer(), integer(), string()) -> ok.
tb_print(_X, _Y, _Fg, _Bg, _Str) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the termbox input mode.
-spec tb_set_input_mode(integer()) -> integer().
tb_set_input_mode(_Mode) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the termbox output mode.
-spec tb_set_output_mode(integer()) -> integer().
tb_set_output_mode(_Mode) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the default foreground and background attributes used by tb_clear().
-spec tb_set_clear_attrs(integer(), integer()) -> ok.
tb_set_clear_attrs(_Fg, _Bg) ->
    erlang:nif_error(nif_library_not_loaded). 