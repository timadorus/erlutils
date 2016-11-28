%% @author sage
%% @doc generate a tree, allowing for incremental generation
%%
%%

-module(teu_tree_gen).


-include_lib("eunit/include/eunit.hrl").

%% init_control/1
%% --------------------------------------------------------------------
%% @ doc initialize an tree generator.
%% @ end
-callback init_control(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% init_gen/1
%% --------------------------------------------------------------------
%% @ doc initialize an tree generator.
%% @ end
-callback init_gen(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% generate_node/2
%% --------------------------------------------------------------------
%% @ doc generate the next node.
%%
%% @param Parent parent node information
%% @param Info additional information provided by the controler to the generator.
%% @ end
-callback generate_node(Parent :: term(), Info :: term()) ->
        {ok, Node :: term(), State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

-record(node_info, { state :: pending | complete | leaf
                   , data :: term()  %% set by the generator
                   }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


