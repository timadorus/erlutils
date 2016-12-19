%%
%% @doc test module teu_tree_gen
%%
%% @author sage
%%
%% @copyright 2009-2015 Timadorus Project

-module(teu_tree_gen_tests).

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

api_test_() ->
    { "test behavior api",
      setup,

      fun() ->
              %%     application:start(sasl),
              ok
      end,

      fun(_Args) ->
              %%     application:stop(sasl),
              ok
      end,
      fun(_Foo) -> [ ?_test(test_init())
                   ]
      end }.


-define(TREE_IMPL_MODULE, teu_simple_tree_impl).

%%
%% Local Functions
%%

test_init() ->

    Options = [ {generator, teu_iterative_deep_tree_gen}
              ],

    _Ret = teu_tree_gen:init(Options),

    ok.


