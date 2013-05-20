%%
%% @doc TODO: Add description to teu_nodes_tests
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_nodes_tests).

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

info_test_() ->
    { setup, fun() -> ok end,
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

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
                    ?_test(test_read_config())
                   ]
      end }.



%%
%% Local Functions
%%
test_read_config() ->
	?assertEqual({ok, 2}, teu_application:get_env(test_dummy_par1, 3)),
	?assertEqual({ok, 3}, teu_application:get_env(test_dummy_par2, 3)),

	?assertEqual({ok, 2}, teu_application:get_env(par1, teu_app_test, 3)),
	?assertEqual({ok, 3}, teu_application:get_env(par2, teu_app_test, 3)),

