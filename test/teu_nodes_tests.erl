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
                    ?_test(test_split_node()),
					?_test(test_make_numbered_nodes())
                   ]
      end }.



%%
%% Local Functions
%%
test_split_node() ->
    ?assertMatch({"foo","bar"}, teu_nodes:split_node('foo@bar')),
    ?assertMatch({foo, bar}, teu_nodes:split_node_to_atom('foo@bar')).

test_make_numbered_nodes() ->
	?assertEqual(['node1@hosta', 'node2@hostb', 'node3@hosta'],
				 teu_nodes:make_numbered_nodes(node, 1, 3, [hosta, hostb])),
	{ok, LocalHost} = inet:gethostname(),
	Node = list_to_atom("node1@" ++ LocalHost), 
	?debugHere,
	?assertEqual([Node], teu_nodes:make_numbered_nodes(node, 1, 1)),
	ok.