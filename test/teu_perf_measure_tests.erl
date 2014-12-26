%% @author sage
%% @doc unit tests for module teu_perf_measure


-module(teu_perf_measure_tests).

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

initial_test_() ->
    { "some initial tests",
      setup,

      fun() ->
              %%     application:start(sasl),
              ok
      end,

      fun(_Args) ->
              %%     application:stop(sasl),
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_test_calc())
                   ]
      end }.



%%
%% Local Functions
%%
test_test_calc() ->
	 _Ret = teu_perf_measure:test_calc("A Test", [1,2,3,4,5,6,7,8,9,10]),
	ok.
