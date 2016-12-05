%% @author sage
%% @doc @todo Add description to teu_iterative_deep_tree_gen.


-module(teu_iterative_deep_tree_gen).

-behaviour(teu_tree_gen).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


%% ====================================================================
%% Behavior functions
%% ====================================================================
-export([init_ctrl/1, init_gen/1, gen_node/2]).

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
-spec init_ctrl(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init_ctrl(_Args) -> {ok, #ctrl_state{}}.


%% init_gen/1
%% --------------------------------------------------------------------
%% @ doc initialize a tree generator.
%% @ end
-spec init_gen(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% gen_node/2
%% --------------------------------------------------------------------
%% @ doc generate the next node.
%%
%% @param Parent parent node information
%% @param Info additional information provided by the controler to the generator.
%% @ end
-spec gen_node(Parent :: term(), Info :: term()) ->
        {ok, Node :: term(), State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------


%% ====================================================================
%% Internal functions
%% ====================================================================


