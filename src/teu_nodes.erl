%%
%% @doc functions for managing nodes
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_nodes).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([split_node/1, split_node_to_atom/1]).

%%
%% API Functions
%%

%% @doc slit the name of a node into node- and host part.
-spec split_node_to_atom(NodeAndHost::atom()) -> {NodeName::atom(), HostName::atom()}.
split_node_to_atom(NodeAndHost) ->
    {Node, Host} = split_node(NodeAndHost),
    {list_to_atom(Node), list_to_atom(Host)}.

%% @doc slit the name of a node into node- and host part.
-spec split_node(Node::atom()|string()) -> {NodeName::string(), HostName::string()}.
split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).



%%
%% Local Functions
%%

