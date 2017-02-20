%%
%% @doc helper event handler for the function @see teu_procs:wait_for_event/1
%% Dont use directly!
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%

-module(teu_procs_helper_handler).
-behaviour(gen_event).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {event :: term(), caller :: pid()}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:init-1">gen_event:init/1</a>
-spec init(InitArgs) -> Result when
  InitArgs :: Args | {Args, Term :: term()},
  Args :: term(),
  Result :: {ok, State}
      | {ok, State, hibernate}
      | {error, Reason :: term()},
  State :: term().
%% ====================================================================
init([Event, Caller]) ->
    {ok, #state{event = Event, caller = Caller}}.


%% handle_event/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_event-2">gen_event:handle_event/2</a>
-spec handle_event(Event :: term(), State :: term()) -> Result when
  Result :: {ok, NewState}
      | {ok, NewState, hibernate}
      | {swap_handler, Args1, NewState, Handler2, Args2}
      | remove_handler,
  NewState :: term(), Args1 :: term(), Args2 :: term(),
  Handler2 :: Module2 | {Module2, Id :: term()},
  Module2 :: atom().
%% ====================================================================
handle_event(CurrEvent, State)
  when CurrEvent == State#state.event
    ->
    gen_server:cast(State#state.caller, {have_seen_event, CurrEvent}),

    remove_handler.


%% handle_call/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_call-2">gen_event:handle_call/2</a>
-spec handle_call(Request :: term(), State :: term()) -> Result when
  Result :: {ok, Reply, NewState}
      | {ok, Reply, NewState, hibernate}
      | {swap_handler, Reply, Args1, NewState, Handler2, Args2}
      | {remove_handler, Reply},
  Reply :: term(),
  NewState :: term(), Args1 :: term(), Args2 :: term(),
  Handler2 :: Module2 | {Module2, Id :: term()},
  Module2 :: atom().
%% ====================================================================
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_info-2">gen_event:handle_info/2</a>
-spec handle_info(Info :: term(), State :: term()) -> Result when
  Result :: {ok, NewState}
      | {ok, NewState, hibernate}
      | {swap_handler, Args1, NewState, Handler2, Args2}
      | remove_handler,
  NewState :: term(), Args1 :: term(), Args2 :: term(),
  Handler2 :: Module2 | {Module2, Id :: term()},
  Module2 :: atom().
%% ====================================================================
handle_info(_Info, State) ->
    {ok, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:terminate-2">gen_event:terminate/2</a>
-spec terminate(Arg, State :: term()) -> term() when
  Arg :: Args
    | {stop, Reason}
    | stop
    | remove_handler
    | {error, {'EXIT', Reason}}
    | {error, Term :: term()},
  Args :: term(), Reason :: term().
%% ====================================================================
terminate(_Arg, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:code_change-3">gen_event:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> {ok, NewState :: term()} when
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


