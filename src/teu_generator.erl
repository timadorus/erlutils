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

%% make_work/1
%% --------------------------------------------------------------------
%% @doc create a new work package to be sent to one of the generators.
-callback make_work(State :: term()) -> 
           {ok, WorkRef :: reference(), WorkSpec :: term(), State :: term()} 
         | {error, Reason :: term()}.
%% --------------------------------------------------------------------

%% deliver_work/3
%% @doc hand in resultant work by the generator to the controler.
%% @end
-callback deliver_work(GenRef :: reference(), Result :: term(), 
                       State :: term()) -> 
             {ok, State :: term()} | {error, Reason ::term()}.
%% --------------------------------------------------------------------

%%
%%  Worker functions
%%

%% generate/3
%% --------------------------------------------------------------------
%% @doc generate a number of new Elements.
%% @param WorkRef an identifier for the work package. As the process of the 
%%                generator may change to to failures, this is a separate 
%%                reference.
%% @param WorkSpec Information for the generator to do its work.
%% @param Pid of the control process. This is provided, so it does not have to be stored
%%        in the state of each generator.
%% @returns a list of new elements. 
%% @end
-callback generate(WorkRef :: reference(), WorkSpec :: term, CtrlPid :: pid()) ->
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
%%  Options: 
%%   <table>
%%     <tr><th>Options</th><th>Type</th><th>Description</th><th>default</th></tr>
%%     <tr>
%%       <td>overrun_warning</td>
%%       <td>infinity | integer() >= 1</td>
%%       <td></td>
%%       <td>infinity</td>
%%     </tr>
%%     <tr>
%%       <td>overrun_handler</td>
%%       <td>{Module :: atom(), Fun :: atom()}}</td>
%%       <td></td>
%%       <td>undefined</td>
%%     </tr>
%%     <tr>
%%       <td>workers</td>
%%       <td>integer() >= 1</td>
%%       <td>Number of workers to start</td>
%%       <td>100</td>
%%     </tr>
%%     <tr>
%%       <td>worker_opt</td>
%%       <td>gen:options()</td>
%%       <td></td>
%%       <td>[]</td>
%%     </tr>
%%     <tr>
%%       <td>control_opt</td>
%%       <td>gen:options()</td>
%%       <td></td>
%%       <td>[]</td>
%%     </tr>
%%     <tr>
%%       <td>worker</td>
%%       <td>{Module :: atom(), InitArg :: term()}}</td>
%%       <td></td>
%%       <td>undefined</td>
%%     </tr>
%%     <tr>
%%       <td>strategy</td>
%%       <td>supervisor:strategy()</td>
%%       <td></td>
%%       <td>one_for_all</td>
%%     </tr>
%%     <tr>
%%       <td>pool_sup_intensity</td>
%%       <td>integer() >= 0</td>
%%       <td></td>
%%       <td>5</td>
%%     </tr>
%%     <tr>
%%       <td>pool_sup_period</td>
%%       <td>integer() >= 0</td>
%%       <td></td>
%%       <td>60</td>
%%     </tr>
%%   </table>
%% @params Options can contain
%%     Options for the worker gen_server processs are in worker_opt, those 
%%     any option to gen_server (for the control 
%% server). Also: {gen_count, N} for a number of generators. The default
%% is 5.
%% @end
-spec start_link(Module :: atom(), Arguments :: list(), Options :: list()) -> 
          {ok, ControlerPid :: pid()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
start_link(Module, Arguments, Options) ->
    TrueOpts = teu_application:set_opts([ {worker_type, gen_server}
                                        , {worker,{teu_generator_wrk, [Module]}}
                                        ], 
                            Options),
    CtrlOptions = teu_application:opt(control_opt, TrueOpts, []),
    gen_server:start_link(?MODULE, [Module, Arguments, TrueOpts], CtrlOptions).

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
init([Module, CtrlArguments, Options]) ->
    {ok, CtrlState, GenStartArgs} = Module:init(CtrlArguments),
    {ok, #state{module = Module, ctrl_state = CtrlState, 
                options = Options, gen_start_args = GenStartArgs}}.

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
    GenCount = teu_application:opt(gen_count, State#state.options, 5),
    NewState = forN(GenCount, )
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
    Module = State#state.module,
    {ok, GenPid} = teu_generator_wrk:start_link(Module, []),
    NewPidSet = sets:add_element(GenPid, State#state.gen_pids),
    {ok, WorkRef, WorkSpec, CtrlState} = Module:make_work(State#state.ctrl_state),

    teu_generator_wrk:do_work(GenPid, WorkRef, WorkSpec),

    State#state{gen_pids = NewPidSet, ctrl_state = CtrlState}.
