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
-export([get_env/2, get_env/3, remote_start/2, remote_start/3, opt/2, opt/3]).

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


%% opt/3
%% ====================================================================
%% @doc retrieve a named option from an option list.
%%
%% <p>The options must be given as list of name-value pairs. If only the name
%% is given, it may serve as flag for a boolean value.</p>
%%
-spec opt(Name::atom(), Options::[{atom(), term()}|atom()], Default::term()) -> term().
opt(Op, [{Op, Value}|_],_Default) ->
    Value;
opt(Op, [Op|_],_Default) ->
    true;
opt(Op, [_|Options],Default) ->
    opt(Op, Options,Default);
opt(_, [],Default) ->
    Default.

%% opt/2
%% ====================================================================
%% @doc handle a key-value map as the option directives
%%
%% <p>this function uses the atom false as the default value. Otherwise
%% is is the same as opt/3
-spec opt(Option::atom(), OptionList::[{atom(), term()}]) -> term().
opt(Op, Options) -> opt(Op, Options, false). 


%% ====================================================================
%% Internal functions
%% ====================================================================


