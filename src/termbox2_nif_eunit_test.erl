-module(termbox2_nif_eunit_test).
-include_lib("eunit/include/eunit.hrl").

-compile({nowarn_unused_function, [simple_test/0]}).

-spec simple_test() -> ok.
simple_test() ->
    ?assertEqual(1, 1).
