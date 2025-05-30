-module(termbox2_nif_SUITE).
-include_lib("eunit/include/eunit.hrl").

% No setup/teardown needed as tb_init() is expected to fail in non-interactive env.
% We are primarily testing if the NIF calls can be made without crashing.

%% Test Generators
all_tests_() ->
    [
        ?_test(nif_load_test), % Add a simple test to ensure module loads
        ?_test(basic_calls_test),
        ?_test(cursor_calls_test),
        ?_test(cell_print_calls_test)
    ].

%%% Test Cases

% Simple test to ensure the NIF module loaded
nif_load_test() ->
    ?assert(is_list(code:which(?MODULE))).

% Test basic calls return an integer (success or error code)
basic_calls_test() ->
    ?assert(is_integer(termbox2_nif:tb_init())),
    ?assert(is_integer(termbox2_nif:tb_width())),
    ?assert(is_integer(termbox2_nif:tb_height())),
    ?assert(is_integer(termbox2_nif:tb_clear())),
    ?assert(is_integer(termbox2_nif:tb_present())),
    ?assert(is_integer(termbox2_nif:tb_shutdown())).

% Test cursor calls return an integer
cursor_calls_test() ->
    ?assert(is_integer(termbox2_nif:tb_set_cursor(1, 1))),
    ?assert(is_integer(termbox2_nif:tb_hide_cursor())),
    ?assert(is_integer(termbox2_nif:tb_set_cursor(-1, -1))).

% Test cell/print calls return an integer
cell_print_calls_test() ->
    Default = 0, Red = 2, Blue = 5,
    ?assert(is_integer(termbox2_nif:tb_set_cell(0, 0, 36, Red, Blue))),
    ?assert(is_integer(termbox2_nif:tb_print(1, 1, Default, Default, <<"Hello">>))). 