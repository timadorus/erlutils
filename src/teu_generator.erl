%% @author sage
%% @doc behavior for generator pattern list generation

%% Implementation Note: do result list as div-list. consider list structure as
%% tree. build dif-tree to limit work on tree elements that still have active
%% parts.
%%
%% Option 1: spawn one process per node. process sleeps until children return. 
%%           When all children have returned, report resultant list to parent.
%% Option 2: fewer processes run over div-list over and over. Leafs are marked
%%           as such, state of completed subtrees is communicated upward, thus 
%%           resolving need to touch it.
%% 

-module(teu_generator).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% init/1
%% --------------------------------------------------------------------
%% @doc initialize a controler.
%% @end
-callback init(Args :: [term()]) -> 
          {ok, GenState :: term()} 
        | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% init_generator/2
%% --------------------------------------------------------------------
%% @doc initialize a generator.
%% @param ControlPid the Pid of the controler for this generator
%% @end
-callback init_generator(Args :: [term()], ControlPid :: pid()) -> 
          {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------


%% generate/3
%% --------------------------------------------------------------------
%% @doc generate a number of new Elements.
%% @param Resource should gives an indication of how many new elements are to be created. 
%%        This may be the actual number of elements or a total cost of all new elements.
%% @param Pid of the control process. This is provided, so it does not have to be stored
%%        in the state of each generator.
%% @param State is the internal state of the generator.
%% @returns a list of new elements. 
%% @end
-callback generate(Chunks :: term, CtrlPid :: pid(), State :: term()) ->
        {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% API exports
-export([start_link/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% function only exported for tests
-ifdef(TEST).
-export([
        ]).
-endif.

-record(state, { module :: atom()
               , ctrl_state :: term()
               }).

%% ====================================================================
%% API functions
%% ====================================================================

%% start_link/3
%% --------------------------------------------------------------------
%% @doc start a generator/controler set up
%% @end
-spec start_link(Module :: atom(), Arguments :: list(), Options :: list()) -> 
          {ok, ControlerPid :: pid()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
start_link(Module, Arguments, Options) ->
    gen_server:start_link(?MODULE, [Module, Arguments], Options).

%% stop/1
%% --------------------------------------------------------------------
%% @doc stop the generator.
%% @end
%% --------------------------------------------------------------------
-spec stop(Pid::pid()) -> ok.
stop(Pid) -> gen_server:cast(Pid, stop).

%% ====================================================================
%% Behavior functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Module, CtrlArguments]) ->
    CtrlState = Module:init(CtrlArguments),
    {ok, #state{module = Module, ctrl_state = CtrlState}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================


