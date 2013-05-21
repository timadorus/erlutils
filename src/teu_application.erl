%% @author sage
%% @doc @todo Add description to teu_application.


-module(teu_application).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-type restart_type() :: permanent | transient | temporary.
  
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_env/2, get_env/3, remote_start/2, remote_start/3]).

%% get_env/2
%% ====================================================================
%% @doc retrieve configuration value, returning provided default value,
%%  if no value could be found in application configuration. 
%%  (see application:get_env/1 and application:get_env/2 for more details).
-spec get_env(Par::atom(), Default::any()) -> {ok, Value::any()}.
get_env(Par, Default) -> 
	case application:get_env(Par) of
		{ok, Value} -> {ok, Value};
        _ ->           {ok, Default}
	end.

%% get_env/3
%% ====================================================================
%% @doc retrieve configuration value, returning provided default value,
%%  if no value could be found in application configuration. 
%%  (see application:get_env/1 and application:get_env/2 for more details).
-spec get_env(Par::atom(), App::atom(), Default::any()) -> {ok, Value::any()}.
get_env(Par, App, Default) -> 
	case application:get_env(Par, App) of
		{ok, Value} -> {ok, Value};
        _ ->           {ok, Default}
	end.


%% remote_start/2
%% ====================================================================
%% @doc start application on a (already running) remote node
%%
-spec remote_start(Node::atom(), Application::atom()) -> ok | {error, Reason::term()}.
remote_start(Node, Application) -> remote_start(Node, Application, temporary).

%% remote_start/3
%% ====================================================================
%% @doc start application on a (already running) remote node
%%
-spec remote_start(Node::atom(), Application::atom(), Type::restart_type()) -> ok | {error, Reason::term()}.
remote_start(Node, Application, Type) -> 
   rpc:block_call(Node, application, start, [Application, Type]).

%% ====================================================================
%% Internal functions
%% ====================================================================


