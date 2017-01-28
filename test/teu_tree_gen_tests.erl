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
                   , ?_test(test_add_generator())
                   ]
      end }.


process_test_() ->
    { "run tests with actual processes",
      setup,

      fun() ->
              %%     application:start(sasl),
              ok
      end,

      fun(_Args) ->
              %%     application:stop(sasl),
              ok
      end,
      fun(_Foo) -> [ ?_test(test_start())
                   , ?_test(test_start_link())
                   , ?_test(test_add_generator())
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


test_add_generator() ->
    Options = [ {generator, teu_iterative_deep_tree_gen}
              ],

    TGen = teu_tree_gen:init(Options),

    {ok, GPid} = teu_tree_gen:add_generator(TGen,[]),

    ok.

test_start() -> 
    Options = [ {generator, teu_iterative_deep_tree_gen}
              ],

    {ok, CtrlPid} = teu_tree_gen:start(Options),
    ?assert(is_pid(CtrlPid)),
    
    ok = teu_tree_gen:stop(CtrlPid),
    
    teu_procs:wait_for_exit(CtrlPid),
    
   ok.

test_start_link() -> 
    Options = [ {generator, teu_iterative_deep_tree_gen}
              ],

    {ok, CtrlPid} = teu_tree_gen:start(Options),
    ?assert(is_pid(CtrlPid)),

    unlink(CtrlPid),
    ok = teu_tree_gen:stop(CtrlPid),
    
    teu_procs:wait_for_exit(CtrlPid),
    
    ok.
