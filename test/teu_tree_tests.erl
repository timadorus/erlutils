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
                   , ?_test(test_get_child())
                   , ?_test(test_add_child())
                   , ?_test(test_equal())
                   , ?_test(test_delete_child())
                   , ?_test(test_set_get_data())
                   ]
      end }.


-define(TREE_IMPL_MODULE, teu_simple_tree_impl).

%%
%% Local Functions
%%
-record(tree, { module :: atom()
              , root   :: term()
              }).

test_new() ->

    Tree = teu_tree:new(?TREE_IMPL_MODULE, []),
    ?assertMatch(T when is_record(T, tree), Tree),

    ok.


test_get_child() ->

    Tree = teu_tree:new(?TREE_IMPL_MODULE, []),
    Child = teu_tree:new(?TREE_IMPL_MODULE, []),
    NewTree = teu_tree:add_child(Tree, Child, 1),

    ?assertEqual(Child, teu_tree:get_child(NewTree, 1)),

    ThirdTree = teu_tree:add_child(NewTree, Child, 3),

    ?assertEqual(undefined, teu_tree:get_child(ThirdTree, 2)),

    ?assertEqual(undefined, teu_tree:get_child(ThirdTree, 4)),

    ?assertEqual(Tree, teu_tree:get_child(Tree, [])),

    ?assertEqual(Tree, teu_tree:get_child(NewTree, [1])),

    ?assertEqual(undefined, teu_tree:get_child(Tree, [1])),

    ?assertEqual(undefined, teu_tree:get_child(NewTree, [1,1])),

    ?assertThrow(bad_args, teu_tree:get_child(Tree, [1,1])),

    ok.


test_add_child() ->

    Tree = teu_tree:new(?TREE_IMPL_MODULE, []),
    Child = teu_tree:new(?TREE_IMPL_MODULE, []),

    NewTree = teu_tree:add_child(Tree, Child, 1),
    ?assertEqual([Child], teu_tree:children(NewTree)),

    ?assertEqual(Child, teu_tree:add_child(Tree, Child, [])),

    NewTree = teu_tree:add_child(Tree, Child, [1]),
    ?assertEqual([Child], teu_tree:children(NewTree)),

    DeepTree = teu_tree:add_child(Tree, Child, [1,1]),
    ?assertEqual(Child, teu_tree:get_child(DeepTree, [1,1])),

    ok.


test_equal() ->
    Tree1 = teu_tree:new(?TREE_IMPL_MODULE, []),
    Tree2 = teu_tree:new(?TREE_IMPL_MODULE, []),


    ?assertEqual(true, teu_tree:equal(Tree1, Tree1)),

    Tree3 = teu_tree:add_child(Tree1, Tree2, 2),
    ?assertEqual(false, teu_tree:equal(Tree1, Tree3)),

    Tree4 = teu_tree:delete_child(Tree3, 2),
    ?assertEqual(true, teu_tree:equal(Tree1, Tree4)),

    ok.


test_delete_child() ->
    Tree = teu_tree:new(?TREE_IMPL_MODULE, []),
    Child = teu_tree:new(?TREE_IMPL_MODULE, []),
    NewTree = teu_tree:add_child(Tree, Child, 1),

    ?assert(teu_tree:equal(NewTree, teu_tree:delete_child(NewTree, 2))),

    ?assert(teu_tree:equal(Tree, teu_tree:delete_child(NewTree, 1))),

    ok.


test_set_get_data() ->
    Tree1 = teu_tree:new(?TREE_IMPL_MODULE, []),
    Val = a_value,

    Tree2 = teu_tree:set_data(Tree1, Val),
    Ret = teu_tree:get_data(Tree2),

    ?assertEqual(Val, Ret),

    ok.