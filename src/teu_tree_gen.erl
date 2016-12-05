%% @author sage
%% @doc generate a tree, allowing for incremental generation
%% This behavovior allows the central control of a number of worker process
%% incrementally processing a tree
%%

-module(teu_tree_gen).


-include_lib("eunit/include/eunit.hrl").

%% init_ctrl/1
%% --------------------------------------------------------------------
%% @ doc initialize a generator controler.
%% @ end
-callback init_ctrl(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% init_gen/1
%% --------------------------------------------------------------------
%% @ doc initialize a tree generator.
%% @ end
-callback init_gen(Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% gen_node/2
%% --------------------------------------------------------------------
%% @ doc generate the next node.
%%
%% @param Parent parent node information
%% @param Info additional information provided by the controler to the generator.
%% @ end
-callback gen_node(Parent :: term(), Info :: term()) ->
        {ok, Node :: term(), State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% -record(node_info, { state :: pending | complete | leaf
%%                    , data :: term()  %% set by the generator
%%                    }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3]).

%% start/3
%% --------------------------------------------------------------------
%% @doc start generator systm
%% this will start at least two processes (one controler and one worker),
%% but may start an arbitrary number more, depending on the options.
%%
%% Options are:
%% <dl>
%% <dt><code>generator</code></dt><dd>Module implementing the generator (atom(), Default: )<dd>
%% <dt><code>gen_args</code></dt><dd>Arguments for the generator/controler (Default: [])<dd>
%% </dl>
%%
%% @param Options a map of the configuration
%%
%%
%% @param GenModule name of the module to use for the generator controler
%%                  (implementing teu_tree_gen behaviour).
%% @param GenArgs additional arguments for the controler.
%% @param TreeImpl  name of the module implementing the tree.
%% @param TreeArgs additional arguemtns of the tree implementation
%% @param
-spec start(GenModule :: atom(), GenArgs :: term(),
            TreeImpl :: atom(), TreeArgs :: term(),
            Options :: #{}).
%% --------------------------------------------------------------------
start()

%% ====================================================================
%% Internal functions
%% ====================================================================


