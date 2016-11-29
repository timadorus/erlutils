%%
%% @doc TODO: Add description to teu_nodes_tests
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_tree_tests).

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
      fun(_Foo) -> [ ?_test(test_new())
                   , ?_test(test_equal())
                   , ?_test(test_add_child())
                   , ?_test(test_get_child())
                   , ?_test(test_delete_child())
                   ]
      end }.



%%
%% Local Functions
%%
-record(tree, { module :: atom()
              , root   :: term()
              }).

test_new() ->

    Tree = teu_tree:new(teu_simple_tree_impl, []),
    ?assertMatch(T when is_record(T, tree), Tree),

    ok.


test_equal() ->
    Tree1= teu_tree:new(teu_simple_tree_impl, []),
    Tree2= teu_tree:new(teu_simple_tree_impl, []),

    ?assertEqual(true, teu_tree:equal(Tree1, Tree1)),

    ?assertEqual(true, teu_tree:equal(Tree1, Tree2)),

    ok.


test_add_child() ->

    Tree= teu_tree:new(teu_simple_tree_impl, []),
    Child= teu_tree:new(teu_simple_tree_impl, []),
    NewTree = teu_tree:add_child(Tree, Child, 1),

    ?assertEqual([Child], teu_tree:children(NewTree)),

    ok.

test_get_child() ->

    Tree= teu_tree:new(teu_simple_tree_impl, []),
    Child= teu_tree:new(teu_simple_tree_impl, []),
    NewTree = teu_tree:add_child(Tree, Child, 1),

    ?assertEqual(Child, teu_tree:get_child(NewTree, 1)),

    ThirdTree = teu_tree:add_child(NewTree, Child, 3),

    ?assertEqual(undefined, teu_tree:get_child(ThirdTree, 2)),

    ?assertEqual(undefined, teu_tree:get_child(ThirdTree, 4)),


    ok.


test_delete_child() ->
    Tree= teu_tree:new(teu_simple_tree_impl, []),
    Child= teu_tree:new(teu_simple_tree_impl, []),
    NewTree = teu_tree:add_child(Tree, Child, 1),

    ?assertEqual(Tree, teu_tree:delete_child(Tree, 2)),

    ?assertEqual(Tree, teu_tree:delete_child(NewTree, 1)),

    ok.
