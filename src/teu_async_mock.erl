%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 12.06.2013
%%% -------------------------------------------------------------------
-module(teu_async_mock).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1, start/1, stop/1,
         last_message/1, message_stack/1, wait_for_msg/1, wait_for_msg/2,
         register_reply/3, unregister_reply/2,
         check_for_msg/2, is_verbose/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% function only exported for tests
-ifdef(TEST).
-export([
         check_replies/2
        ]).
-endif.

-record(state, {lastMessage                        :: term(),
                messageStack = []                  :: [term()],
                opt_verbose = false                :: boolean(),
                observer = none                    :: pid() | none,
                replies = dict:new()               :: dict:dict(),
                observerPred = fun(_) -> true end  :: fun()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% start_link/1
%% --------------------------------------------------------------------
%% @doc start the mock.
%% Options takes a list of options, which may be one of:
%% <dl>
%%  <dt>register</dt><dd>register server to name <tt>teu_async_mock</tt></dd>
%%  <dt>{register, atom()}</dt><dd>register server to name given by atom</dd>
%%  <dt>verbose</dt><dd>control verbosity of debug messages.</dd>
%% </dl>
%% @end
%% --------------------------------------------------------------------
%% FIXME: do a proper error/exception handling if the value of the option does
%%        not make sense (e.g. something other than an atom)
-spec start_link(Options::[{register, atom()} | register | verbose]) ->
          {ok, Pid::pid()} | ignore | {error, Error::any()}.
start_link(Options) ->
    RegisterName = teu_application:opt(register, Options),
    case RegisterName of
        true ->      gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []);
        false ->     gen_server:start_link(?MODULE, [Options], []);
        RegisterName ->
           if
            is_atom(RegisterName) -> gen_server:start_link({local, RegisterName}, ?MODULE, [Options], []);
            true -> gen_server:start_link(?MODULE, [Options], [])
           end
    end.

%% start/1
%% --------------------------------------------------------------------
%% @doc start the mock.
%% if Register is set to 'register' the process will register locally with the name 'quperl_control'.
%% @end
%%
%% FIXME: do a proper error/exception handling if the value of the option does
%%        not make sense (e.g. something other than an atom)
%%
-spec start(Options::[{register, atom()} | register | verbose]) ->
		  {ok, Pid::pid()} | ignore | {error, Error::any()}.
%% --------------------------------------------------------------------
start(Options) ->
    RegisterName = teu_application:opt(register, Options),
    case RegisterName of
        true ->      gen_server:start({local, ?MODULE}, ?MODULE, [Options], []);
        false ->     gen_server:start(?MODULE, [Options], []);
        RegisterName ->
           if
            is_atom(RegisterName) -> gen_server:start({local, RegisterName}, ?MODULE, [Options], []);
            true -> gen_server:start(?MODULE, [Options], [])
           end
    end.


%% last_message/1
%% --------------------------------------------------------------------
%% @doc return the last message recieved by the mock.
%% @end
-spec last_message(Pid::pid()) -> term().
%% --------------------------------------------------------------------
last_message(Pid) ->
    gen_server:call(Pid, get_last).

%% message_stack/1
%% --------------------------------------------------------------------
%% @doc return the messages retrieved by the mock, as a list, last message first.
%% @end
-spec message_stack(Pid::pid()) -> term().
%% --------------------------------------------------------------------
message_stack(Pid) ->
    gen_server:call(Pid, get_stack).

%% is_verbose/1
%% --------------------------------------------------------------------
%% @doc return true if verbose message reporting is activated, false
%% otherwise.
%% @end
-spec is_verbose(Pid::pid()) -> true | false.
%% --------------------------------------------------------------------
is_verbose(Pid) ->
    gen_server:call(Pid, is_verbose).

%% wait_for_msg/1
%% --------------------------------------------------------------------
%% @doc block until any message is recieved by the async_mock process.
%% @end
%% --------------------------------------------------------------------
-spec wait_for_msg(Pid) -> {ok, Message} | {error, timeout} when
     Pid :: pid(),
     Message :: term().
%% --------------------------------------------------------------------
wait_for_msg(Pid) -> wait_for_msg(Pid, fun(_) -> true end).

%% wait_for_msg/2
%% --------------------------------------------------------------------
%% @doc block until a message matching the argument is recieved by the
%% async_mock process.
%%
%% The message is matched using a predicate that will return true for
%% acceptable messages, false otherwise.
%% @end
%% --------------------------------------------------------------------
-spec wait_for_msg(Pid, FilterPred) -> {ok, Message} | {error, timeout} when
     Pid :: pid(),
     FilterPred :: fun(),
     Message :: term().
%% --------------------------------------------------------------------
wait_for_msg(Pid, FilterPred) ->
    gen_server:cast(Pid, {teu_async_mock_internal_wait_for_msg, FilterPred, self()}),

    receive
        {received_msg, Msg} ->
            gen_server:cast(Pid, {teu_async_mock_internal_received_msg, FilterPred, self()}),
            {ok, Msg}
    after
        %% wait 3 seconds
        3000 -> {error, timeout}
    end.


%% check_for_msg/2
%% --------------------------------------------------------------------
%% @doc block until a message matching the argument is recieved by the
%% async_mock process. If the messages has been recieved previously (as
%% indicated by an entry in the call log), the function will return
%% immediately.
%%
%% The message is matched using a predicate that will return true for
%% acceptable messages, false otherwise.
%% @end
-spec check_for_msg(Pid, FilterPred) -> {ok, Message} | {error, timeout} when
     Pid :: pid(),
     FilterPred :: fun(),
     Message :: term().
%% --------------------------------------------------------------------
check_for_msg(Pid, FilterPred) ->
    gen_server:cast(Pid, {teu_async_mock_internal_check_for_msg, FilterPred, self()}),

    receive
        {received_msg, Msg} ->
            gen_server:cast(Pid, {teu_async_mock_internal_received_msg, FilterPred, self()}),
            {ok, Msg}
    after
        %% wait 3 seconds
        3000 -> {error, timeout}
    end.


%% register_reply/3
%% --------------------------------------------------------------------
%% @doc set a reply to give for a synchronous message sent via gen_server:call.
%%
%% Pred is a function that will return true for the wanted message.
%% @end
-spec register_reply(Pid::pid(), Pred :: fun(), Reply :: term()) -> reference().
%% --------------------------------------------------------------------
register_reply(Pid, Pred, Reply) ->
    gen_server:call(Pid, {register_reply, Pred, Reply}).


%% unregister_reply/2
%% --------------------------------------------------------------------
%% @doc remove reply to give for a synchronous message sent via gen_server:call.
%%
%% Ref is the reference returned by the register_reply/3 function.
%% @end
-spec unregister_reply(Pid::pid(), Ref :: reference()) -> ok | {error, invalid_reference}.
%% --------------------------------------------------------------------
unregister_reply(Pid, Ref) ->
    gen_server:call(Pid, {unregister_reply, Ref}).


%% stop/1
%% --------------------------------------------------------------------
%% @doc stop the process
%% @end
%% --------------------------------------------------------------------
-spec stop(Pid::pid()) -> ok.
stop(Pid) -> gen_server:cast(Pid, stop).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Options]) ->
	Verbose = teu_application:opt(verbose, Options),
%% 	?debugFmt("Options: ~p, opt_verbose: ~p~n",[Options, Verbose]),

    {ok, #state{opt_verbose = Verbose}}.

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
handle_call(get_last, _From, State) ->
%%     ?debugFmt("teu_async_mock:handle_call called for: get_last",[]),
    {reply, State#state.lastMessage, State};

handle_call(get_stack, _From, State) ->
%%     ?debugFmt("teu_async_mock:handle_call called for: get_last",[]),
    {reply, State#state.messageStack, State};

handle_call(is_verbose, _From, State) ->
%%     ?debugFmt("teu_async_mock:handle_call called for: is_verbose",[]),
    {reply, State#state.opt_verbose, State};

handle_call({register_reply, Pred, Reply}, _From, State) ->
    Ref = make_ref(),
    NewDict = dict:store(Ref, {Pred, Reply}, State#state.replies),
    {reply, Ref, State#state{replies = NewDict}};

handle_call({unregister_reply, Ref}, _From, State) ->
    NewDict = dict:erase(Ref, State#state.replies),
    {reply, ok, State#state{replies = NewDict}};

handle_call(Request, _From, State) ->
    {reply, check_replies(State#state.replies, Request), State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({teu_async_mock_internal_wait_for_msg, FilterPred, ObserverPid}, State) ->
    {noreply, State#state{observer=ObserverPid, observerPred=FilterPred}};

handle_cast({teu_async_mock_internal_check_for_msg, FilterPred, ObserverPid}, State) ->
    %% check in old messages
    lists:foreach(fun(Msg) ->
                          case FilterPred(Msg) of
                              true -> ObserverPid ! {received_msg, Msg};
                              _ -> ok
                          end
                  end,
                  State#state.messageStack),
    {noreply, State#state{observer=ObserverPid, observerPred=FilterPred}};

handle_cast({teu_async_mock_internal_received_msg, _FilterPred, _ObserverPid}, State) ->
	{noreply, State#state{observer=none}};

handle_cast(Msg, State) ->

	case State#state.opt_verbose of
		true -> io:format("have seen message: ~p\n",[Msg]);
		_    -> ok
    end,

    OldStack = State#state.messageStack,

	%% notify observer
    case State#state.observer of
		Pid when is_pid(Pid) ->
			ObserverPred = State#state.observerPred,
			case ObserverPred(Msg) of
				true -> Pid ! {received_msg, Msg};
                _ -> ok
				end;
	    _ -> ok
	end,

    {noreply, State#state{lastMessage = Msg, messageStack = [Msg|OldStack]}}.


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

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

find_reply(_Ref, Pred, Reply, CurrMsg, false) ->
    case Pred(CurrMsg) of
        true -> Reply;
        _ -> false
    end;
find_reply(_Ref, _Pred, _Reply, _CurrMsg, Out) ->
    Out.

-spec check_replies(Replies :: dict:dict(), Request :: term()) ->
          Reply :: term() | {error, {unknown_message, term()}}.
check_replies(Replies, Request) ->
    case dict:fold(fun(Ref, {Pred, Reply}, Acc) ->
                      find_reply(Ref, Pred, Reply, Request, Acc)
                      end,
                      false, Replies) of
        false ->
            %%     ?debugFmt("unknown message: ~p", [Request]),
            {error, {unknown_message, Request}};
        Reply ->
            Reply
        end.

