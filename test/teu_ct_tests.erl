%% @author sage
%% @doc @todo Add description to teu_ct_tests.


-module(teu_ct_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% Fixtures
%%

%% info_test_() ->
%%     { setup, fun() -> ok end,
%%       fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

internals_test_() ->
    { "test internal functions",
      setup,

      fun() ->
              ok
      end,

      fun(_Args) ->
              ok
      end,
      fun(_Args) -> [ ?_test(test_config())
                   ]
      end }.

test_config() ->
	TestCases = [{foo, apple},
                 {bar, [orange, peach]},
                 {one, [{fruit, [apple, orange]}, {veggi, [{english, potato}, {german, kartoffel}]}]}],

    ?assertEqual(apple, teu_ct:config(foo, TestCases)),
    ?assertEqual([orange, peach], teu_ct:config(bar, TestCases)),
    ?assertEqual([orange, peach], teu_ct:config([bar], TestCases)),
    ?assertEqual([apple, orange], teu_ct:config([one, fruit], TestCases)),
    ?assertEqual(kartoffel, teu_ct:config([one, veggi, german], TestCases)),
    ok.


