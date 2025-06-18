-module(termbox2_nif_windows_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Test Cases

skip_windows_tests() ->
    ?assert(true).

windows_title_test() ->
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title("Short")),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title("Long Title With Spaces")),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title("Special!@#$%^&*()")),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title("")).

windows_position_test() ->
    ?assertMatch({ok, _}, termbox2_nif:tb_set_position(0, 0)),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_position(100, 100)),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_position(800, 600)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_position(-1, 0)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_position(0, -1)).

windows_console_test() ->
    ?assert(is_integer(termbox2_nif:tb_init())),
    ?assert(is_integer(termbox2_nif:tb_width())),
    ?assert(is_integer(termbox2_nif:tb_height())),
    ?assert(is_integer(termbox2_nif:tb_clear())),
    ?assert(is_integer(termbox2_nif:tb_present())),
    ?assert(is_integer(termbox2_nif:tb_shutdown())).

windows_long_unicode_title_test() ->
    LongTitle = lists:duplicate(255, $A),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title(LongTitle)),
    ?assertMatch({ok, _}, termbox2_nif:tb_set_title("测试Unicode标题")),
    OverLong = lists:duplicate(300, $B),
    _ = termbox2_nif:tb_set_title(OverLong),
    ok.

windows_invalid_title_type_test() ->
    ?assertMatch({error, _}, termbox2_nif:tb_set_title(12345)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_title({tuple})),
    ?assertMatch({error, _}, termbox2_nif:tb_set_title(undefined)).

windows_boundary_position_test() ->
    ?assertMatch({ok, _}, termbox2_nif:tb_set_position(32767, 32767)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_position(-32768, 0)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_position(0, -32768)),
    ?assertMatch({error, _}, termbox2_nif:tb_set_position(999999, 999999)).

windows_nif_exports_test() ->
    [ ?assert(erlang:function_exported(termbox2_nif, F, A)) || {F, A} <- [{tb_init,0},{tb_shutdown,0},{tb_width,0},{tb_height,0},{tb_clear,0},{tb_present,0},{tb_set_cursor,2},{tb_hide_cursor,0},{tb_set_cell,5},{tb_set_input_mode,1},{tb_set_output_mode,1},{tb_print,5},{tb_set_title,1},{tb_set_position,2}] ]. 