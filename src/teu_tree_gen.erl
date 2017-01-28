%% @author sage
%% @doc generate a tree, allowing for incremental generation
%% This behavovior allows the central control of a number of worker process
%% incrementally processing a tree
%%

-module(teu_tree_gen).

%% init_ctrl/1
%% --------------------------------------------------------------------
%% @ doc initialize a generator controler.
%% @ end
-callback init_ctrl(Args :: [term()]) -> {ok, Root :: term(), State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% init_gen/1
%% --------------------------------------------------------------------
%% @ doc initialize a tree generator.
%% @ end
-callback init_gen(CtrlState :: term(), Args :: [term()]) -> {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% gen_nodes/2
%% --------------------------------------------------------------------
%% @ doc generate the next nodes.
%% This may generate any number of new nodes. Usually based on the type of tree
%% (e.g. an octree usually will have eight new nodes) and the information in Info.
%%
%% @param Parent parent node information
%% @param Info additional information provided by the controler to the generator.
%% @param GenState current state of the generator.
%% @returns  the node to replace the parent, the list of newly generated end
%%            points and the new State of the Generator
%% @ end
-callback gen_node(Parent :: term(), Info :: term(), GenState :: term()) ->
        {ok, Node :: term(), NewNodes :: [term()], State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% is_final/1
%% --------------------------------------------------------------------
%% @doc determine whether a node is final or needs more processing.
%% --------------------------------------------------------------------
-callback is_final(Node :: term()) -> true | false.
%% --------------------------------------------------------------------

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([ start/1, start_link/1, stop/1
        , add_generator/2
        ]).

%% start/1
%% --------------------------------------------------------------------
%% @doc start generator system.
%%
%% this will start at least two processes (one controler and one worker),
%% but may start an arbitrary number more, depending on the options.
%%
%% Options are:
%% <dl>
%% <dt><code>generator</code></dt><dd>Module implementing the generator (<code>atom()</code>)<dd>
%% <dt><code>gen_args</code></dt><dd>Arguments for the generator/controler (Default: [])<dd>
%% <dt><code>tree_impl</code></dt><dd>Module implementing the tree (<code>atom()</code>, Default: teu_simple_tree_impl)<dd>
%% <dt><code>tree_args</code></dt><dd>Arguments for the tree implementation (Default: [])<dd>
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
%% @returns the pid of the control wrapper.
%% @param
-spec start(Options :: #{}) ->
         {ok, pid()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
start(Options) -> gen_server:start(?MODULE, Options, []).


%% start_link/1
%% --------------------------------------------------------------------
%% @doc start generator system.
%%
%% same as @see start/1, but will link the resultant process.
%% @end
-spec start_link(Options :: #{}) ->
         {ok, pid()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
start_link(Options) -> gen_server:start_link(?MODULE, Options, []).


%% stop/1
%% --------------------------------------------------------------------
%% @doc stop the generator controler and all generators.
%% @end
-spec stop(CtrlPid :: pid()) -> ok.
%% --------------------------------------------------------------------
stop(CtrlPid) -> gen_server:cast(CtrlPid, stop).


%% add_generator/2
%% --------------------------------------------------------------------
%% @doc add a new generator instance.
%% @end
-spec add_generator(CtrlPid :: pid(), Args :: term()) -> 
          {ok, GenPid :: pid()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
add_generator(CtrlPid, Args) -> gen_server:call(CtrlPid, {add_gen, Args}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(tree_gen_info, { tree_impl = teu_simple_tree_impl   :: atom()
                       , tree_args = []                     :: list()
                       , generator                          :: atom()
                       , root_node                          :: term()
                       , ctrl_state                         :: term()
                       , gen_state                          :: term()
                       }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(Options) ->
    TreeImpl = teu_application:opt(tree_impl, Options, teu_simple_tree_impl),
    TreeArgs = teu_application:opt(tree_args, Options, []),

    Generator =
        case  teu_application:opt(generator, Options, undefined) of
            undefined ->
                throw(generator_module_not_defined);
            Gen when is_atom(Gen) ->
                Gen;
            _ ->
                throw(generator_module_not_defined)
        end,

    GenArgs = teu_application:opt(tree_args, Options, []),
    {RootNode, CtrlState} =
        case Generator:init_ctrl(GenArgs) of
            {ok, RN, CS} -> {RN, CS};
            {error, R1} -> throw({gen_init_failed, R1})
        end,


    GenState =
        case Generator:init_gen(CtrlState, GenArgs) of
            {ok, GS} -> GS;
            {error, R2} -> throw({gen_init_failed, R2})
        end,


    {ok, #tree_gen_info{tree_impl = TreeImpl,  tree_args = TreeArgs,
                        generator = Generator,
                        root_node = RootNode,
                        ctrl_state = CtrlState, gen_state = GenState}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call({add_gen, Args}, _From, State) ->
    Generator = State#tree_gen_info.generator,
    CtrlState = State#tree_gen_info.ctrl_state,
    Reply = Generator:init_gen(CtrlState, Args),
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
    {stop, normal, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================



