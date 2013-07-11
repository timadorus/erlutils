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
-export([start_link/1, last_message/1, message_stack/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lastMessage, messageStack}).

%% ====================================================================
%% External functions
%% ====================================================================

%% start_link/1
%% --------------------------------------------------------------------
%% @doc start the mock.
%% if Register is set to 'register' the process will register locally with the name 'quperl_control'.
%% @end
%% --------------------------------------------------------------------
%% FIXME: do a proper error/exception handling if the value of the option does 
%%        not make sense (e.g. something other than an atom)
-spec start_link(Options::[{register, atom()}]) -> {ok, Pid::pid()} | ignore | {error, Error::term()}.
start_link(Options) ->
	RegisterName = teu_application:opt(register, Options),
    case RegisterName of
		true ->      gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
		false ->     gen_server:start_link(?MODULE, [], []); 
        RegisterName ->  
           if 
            is_atom(RegisterName) -> gen_server:start_link({local, RegisterName}, ?MODULE, [], []);
            true -> gen_server:start_link(?MODULE, [], [])
		   end
    end.


%% last_message/1
%% --------------------------------------------------------------------
%% @doc return the last message recieved by the mock.
%% @end
%% --------------------------------------------------------------------
-spec last_message(Pid::pid()) -> term().
last_message(Pid) ->
    gen_server:call(Pid, get_last).

%% message_stack/1
%% --------------------------------------------------------------------
%% @doc return the messages retrieved by the mock, as a list, last message first.
%% @end
%% --------------------------------------------------------------------
-spec message_stack(Pid::pid()) -> term().
message_stack(Pid) ->
    gen_server:call(Pid, get_stack).

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
init([]) ->
    {ok, #state{messageStack=[]}}.

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

handle_call(Request, _From, State) ->
    ?debugFmt("unknown message: ~p", [Request]),
    {reply, {unknown_message, Request}, error, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    ?debugFmt("have seen message: ~p",[Msg]),
    OldStack = State#state.messageStack,
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

