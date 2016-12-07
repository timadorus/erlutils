%% @author sage
%% @doc implement an iterative deeping of tree.


-module(teu_iterative_deep_tree_gen).

-behaviour(teu_tree_gen).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


%% ====================================================================
%% Behavior functions
%% ====================================================================
-export([init_ctrl/1, init_gen/1, gen_nodes/3, is_final/1]).

-record(ctrl_state, { state :: pending | complete | leaf
                   , data :: term()  %% set by the generator
                   }).

-record(node_state, { state :: pending | complete | leaf
                   , data :: term()  %% set by the generator
                   }).

%% init_ctrl/1
%% --------------------------------------------------------------------
%% @ doc initialize a generator control.
%% @ end
-spec init_ctrl(Args :: [term()]) ->
          {ok, Root :: term(), State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init_ctrl(_Args) -> {ok, root, #ctrl_state{}}.


%% init_gen/1
%% --------------------------------------------------------------------
%% @ doc initialize a tree generator.
%% @ end
-spec init_gen(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init_gen(_Args) -> #node_state{}.

%% gen_nodes/2
%% --------------------------------------------------------------------
%% @ doc generate the next node.
%%
%% @param Parent parent node information
%% @param Info additional information provided by the controler to the generator.
%% @ end
-spec gen_nodes(Parent :: term(), Info :: term(), GenState :: term()) ->
        {ok, Nodes :: [term()], State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
gen_nodes(_Parent, _Info, GenState) -> {ok, [], GenState}.

%% is_final/1
%% --------------------------------------------------------------------
%% @doc determine whether a node is final or needs more processing.
%% --------------------------------------------------------------------
-spec is_final(Node :: term()) -> true | false.
%% --------------------------------------------------------------------
is_final(_Node) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================


