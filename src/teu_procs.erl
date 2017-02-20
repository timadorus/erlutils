%%
%% @doc a collection of functions to help handling multiple processes.
%%
%% <p>the module also implements the gen_server behavior, bus should
%% only be started by the functions of the module, as they will also
%% take care of ending the server.</p>
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%

-module(teu_procs).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------
-export([wait_for_exit/1, wait_for_exit/2, wait_for_event/2, wait_for_event/3,
         check_linked/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% wait_for_exit/1
%% --------------------------------------------------------------------
%% @doc wait for the process with the given pid to terminate.
%% @end
-spec wait_for_exit(Pid :: pid()) ->
      ok.
%% --------------------------------------------------------------------
wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    end.

%% wait_for_exit/2
%% --------------------------------------------------------------------
%% @doc wait for the process with the given pid to terminate.
%%
%% function will ensure that the process terminated with the
%% return value given for Reason. If the process has terminated before this
%% function was called, or has never existed, this function will return
%% immediately with 'ok' indicating success.
%% @end
-spec wait_for_exit(Pid :: pid(), Reason :: term()) ->
      ok
        | {error, {expected, Exp :: term()},
              {value,    Val :: term()}}.
%% --------------------------------------------------------------------
 wait_for_exit(Pid, Reason) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, _, _, noproc} ->
            ok;
        {'DOWN', MRef, _, _, Reason} ->
            ok;
        {'DOWN', MRef, _, _, Val} ->
            {error, {expected, Reason}, {value, Val}}
    end.

%% wait_for_event/2
%% --------------------------------------------------------------------
%% @doc wait for th given event to occour, blocking the function until the
%% event is recieved.
%%
%% the function will register a handler with the event manager given, and
%% return if the event is seen.
%%
%% if the event is not seen within 3000 milliseconds, {error, timeout} will
%% be returned
%%
-spec wait_for_event(EventMgrPid :: pid(), Event :: term()) ->
         {ok, Event :: term()} | {error, timeout}.
%% --------------------------------------------------------------------
wait_for_event(EventMgrPid, Event) ->
    wait_for_event(EventMgrPid, Event, 3000).


%% wait_for_event/3
%% --------------------------------------------------------------------
%% @doc wait for th given event to occour, blocking the function until the
%% event is recieved.
%%
%% the function will register a handler with the event manager given, and
%% return if the event is seen.
%%
%% if the event is not seen within a number of milliseconds set by Timeout,
%% {error, timeout} will be returned.
%%
-spec wait_for_event(EventMgrPid :: pid(), Event :: term(), Timeout :: pos_integer()) ->
        {ok, Event :: term()} | {error, timeout}.
%% --------------------------------------------------------------------
wait_for_event(EventMgrPid, Event, Timeout) ->
    {ok, ServerPid} = gen_server:start_link(?MODULE, [], []),
    ok = gen_event:add_handler(EventMgrPid, teu_procs_helper_handler, [Event, ServerPid]),
    {ok, TRef} = timer:send_after(Timeout, ServerPid, timeout),
    gen_server:call(ServerPid, {wait_for_event, TRef}).


%% check_linked/2
%% --------------------------------------------------------------------
%% @doc check whether one process is linked to another.
%%
%% if the process with Pid1 is linked to the process with Pid2 return true, false otherwise.
-spec check_linked(Pid1 :: pid(), Pid2 :: pid()) -> true | false.
%% --------------------------------------------------------------------
check_linked(Pid1, Pid2) when is_pid(Pid1) and is_pid(Pid2) ->
    {links, Links} = process_info(Pid1, links),
    lists:member(Pid2, Links);

check_linked(_Pid1, _Pid2) -> false.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {caller = none :: {pid(), Tag :: term()} | none,
                timer_ref :: timer:tref() | undefined
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
init([]) ->
    {ok, #state{}}.


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
handle_call({wait_for_event, TRef}, From, State) ->
    NewState = State#state{caller = From, timer_ref = TRef},
    {noreply, NewState}.


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
handle_cast({have_seen_event, Event}, State) ->
    gen_server:reply(State#state.caller, {ok, Event}),
    NewState = State#state{caller = none},
    {stop, normal, NewState}.



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
handle_info(timeout, State) ->
    timer:cancel(State#state.timer_ref),
    gen_server:reply(State#state.caller, {error, timeout}),
    NewState = State#state{caller = none},
    {stop, normal, NewState}.


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


