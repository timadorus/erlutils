%% @author sage
%% @doc @todo Add description to teu_application.


-module(teu_application).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_env/2, get_env/3]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================


