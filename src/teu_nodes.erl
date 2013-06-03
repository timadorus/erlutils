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
-export([split_node/1, split_node_to_atom/1, make_node/1, make_node/2]).

%%
%% API Functions
%%

%% @doc slit the name of a node into node- and host part.
%% @end
-spec split_node_to_atom(NodeAndHost::atom()) -> {NodeName::atom(), HostName::atom()}.
split_node_to_atom(NodeAndHost) ->
    {Node, Host} = split_node(NodeAndHost),
    {list_to_atom(Node), list_to_atom(Host)}.

%% @doc slit the name of a node into node- and host part.
%% @end
-spec split_node(Node::atom()|string()) -> {NodeName::string(), HostName::string()}.
split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).

%% @doc make a valid erlang node shortname from Name,
%% using the current (local) hostname
%% @end
-spec make_node(Name::atom()) -> atom().
make_node(Name) ->
    {ok, Hostname} = inet:gethostname(),
    make_node(Name, list_to_atom(Hostname)).

%% @doc make a node shortname from the supplied name and host atoms
%% @end
-spec make_node(Name::atom(), Host::atom()) -> atom().
make_node(Name, Host) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).



%%
%% Local Functions
%%

