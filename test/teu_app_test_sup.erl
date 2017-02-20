%% @author sage
%% @doc @todo Add description to teu_app_test_sup.


-module(teu_app_test_sup).
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

-spec start_link(StartArgs :: list())
    ->  {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(StartArgs) ->
  supervisor:start_link(?MODULE, StartArgs).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) ->
      ignore  | {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% ====================================================================
init([]) ->
    AChild = {'AName', {'AModule', start_link, []},
        permanent, 2000, worker, ['AModule']},
    {ok, {{one_for_all, 0, 1}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


