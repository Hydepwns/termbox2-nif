%% @author DROO AMOR <drew@axol.io>
%% @version 0.1.10

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

%% Helper: Erlang doesn't have string:ends_with/2 until OTP 25, so implement it here
ends_with(Str, Suffix) when is_list(Str), is_list(Suffix) ->
    SuffixLen = length(Suffix),
    StrLen = length(Str),
    case StrLen >= SuffixLen of
        true -> lists:sublist(Str, StrLen - SuffixLen + 1, SuffixLen) =:= Suffix;
        false -> false
    end.

%% @doc Initializes the termbox library. This function should be called before any other functions.
tb_init() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc The library must be finalized using the tb_shutdown() function.  Called only when no more tb_ functions are required.
%% @returns Nothing.
%% @end
tb_shutdown() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in columns
tb_width() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Returns the size of the internal back buffer (which is the same as terminal's window size in rows
tb_height() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Clears the internal back buffer using TB_DEFAULT color.
tb_clear() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Synchronizes the internal back buffer with the terminal by writing to the tty.
tb_present() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the position of the cursor. Upper-left character is (0, 0).
tb_set_cursor(_X, _Y) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Hides the cursor, not every tty supports this.
tb_hide_cursor() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the individual position of the screen at x,y to character ch, with foreground color fg and bg color bg.
tb_set_cell(_X, _Y, _Ch, _Fg, _Bg) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Wait for an event up to timeout_ms milliseconds, returning event details in a tuple.
%% @returns "A THING"
tb_peek_event(_Timeout) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Same as tb_peek_event except no timeout, probably going to beat up erlang scheduler, maybe ?
tb_poll_event() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Print the string at the X and y locations with fg and background colors set.
tb_print(_X, _Y, _Fg, _Bg, _Str) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the termbox input mode.
tb_set_input_mode(_Mode) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the termbox output mode.
tb_set_output_mode(_Mode) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Sets the default foreground and background attributes used by tb_clear().
tb_set_clear_attrs(_Fg, _Bg) ->
    erlang:nif_error(nif_library_not_loaded). 