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
          {ok, CtrlState :: term(), GenStartArgs :: list()} 
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

%% get_work/3
%% @doc request more work from the control process. 
%% This function will asynchronously call the generate/3 function
%% of the correct generator.
%% @end
-callback get_work(GenRef :: reference(), State :: term()) -> 
          ok | {error, Reason ::term()}.
%% --------------------------------------------------------------------

%% deliver_work/3
%% @doc hand in resultant work by the generator to the controler.
%% @end
-callback deliver_work(GenRef :: reference(), Result :: term(), 
                       State :: term()) -> ok | {error, Reason ::term()}.
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
-callback generate(Resource :: term, CtrlPid :: pid(), State :: term()) ->
        {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% API exports
-export([ start_link/3, stop/1
        , run/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% function only exported for tests
-ifdef(TEST).
-export([
         make_gen/1
        ]).
-endif.

-record(state, { module :: atom()
               , ctrl_state :: term()
               , gen_pids :: sets:set()
               , gen_start_args :: list()   %% starting argument for new generators
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


%% run/2
%% --------------------------------------------------------------------
%% @doc execute the generator.
%% @end
-spec run(CtrlPid :: pid(), ExtraArgs :: term()) -> ok.
run(CtrlPid, ExtraArgs) ->
    gen_server:cast(CtrlPid, {run, ExtraArgs}).

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
    {ok, CtrlState, GenStartArgs} = Module:init(CtrlArguments),
    {ok, #state{module = Module, ctrl_state = CtrlState, gen_start_args = GenStartArgs}}.

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

handle_cast({run, _ExtraArgs}, State) ->
    NewState = make_gen(State),
    {noreply, NewState}.


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

%% make_gen/1
%% --------------------------------------------------------------------
%% @doc make new generator.
%% @end
-spec make_gen(State :: #state{}) -> #state{}.
%% --------------------------------------------------------------------
make_gen(State) ->
    {ok, GenPid} = teu_generator_wrk:start_link(State#state.module, State#state.gen_start_args, 
                                                self(), []),
    NewPidSet = sets:add_element(GenPid, State#state.gen_pids),
    State#state{gen_pids = NewPidSet}.
