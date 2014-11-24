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
-export([split_node/1, split_node_to_atom/1, make_node/1, make_node/2, 
		 make_numbered_nodes/3, make_numbered_nodes/4]).

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

%% @doc make a valid erlang node name from Name,
%% using the hostname part of this node. This will either produce a short or 
%% a long name depending on how the node was set up.
%% @end
-spec make_node(Name::atom()) -> atom().
make_node(Name) ->
    {_MyName, Hostname} = split_node(node()),
    make_node(Name, list_to_atom(Hostname)).

%% @doc make a valid erlang node name from Name,
%% using the current (local) hostname
%% @end
-spec make_short_node(Name::atom()) -> atom().
make_short_node(Name) ->
    {ok, Hostname} = inet:gethostname(),
    make_node(Name, list_to_atom(Hostname)).

%% @doc make a node name from the supplied name and host atoms
%% @end
-spec make_node(Name::atom(), Host::atom()) -> atom().
make_node(Name, Host) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

%% @doc make a node shortname from the supplied name, a number and a host atom
%% @end
-spec make_node(Name::atom(), Number::pos_integer(), Host::atom() | string()) -> atom().
make_node(Name, Number, Host) when is_atom(Host)->
	make_node(Name, Number, atom_to_list(Host));

make_node(Name, Number, Host) when is_list(Host)->
    list_to_atom(atom_to_list(Name)
				++ erlang:integer_to_list(Number) 
				++ "@" 
				++ Host).

%% @doc make a number of node names from a numbers range, a label atom.
%% the host name of the local host as retrieved by inet:gethostname will be used.
%% @end
-spec make_numbered_nodes(Label::atom(), StartNum::pos_integer(), EndNum::pos_integer()) ->
		  Nodes::[atom()].
make_numbered_nodes(Label, StartNum, EndNum) ->
	{ok, Hostname} = inet:gethostname(),
	make_numbered_nodes(Label, StartNum, EndNum, [Hostname]).

%% @doc make a number of node names from a numbers range, a label atom and a list of hosts
%% @end
-spec make_numbered_nodes(Label::atom(), StartNum::pos_integer(), EndNum::pos_integer(),
					   Hosts::[atom()]) -> Nodes::[atom()].
make_numbered_nodes(Label, EndNum, EndNum, [StartHost| _]) ->
	[make_node(Label, EndNum, StartHost)];

make_numbered_nodes(Label, StartNum, EndNum, [StartHost | RestHosts]) ->
	[make_node(Label, StartNum, StartHost) | make_numbered_nodes(Label, StartNum+1, EndNum, RestHosts ++ [StartHost])].

%%
%% Local Functions
%%

