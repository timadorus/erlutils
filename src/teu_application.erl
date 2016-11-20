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
-export([get_env/2, get_env/3, remote_start/2, remote_start/3, remote_load/2, 
         remote_set_env/4, remote_set_env/5, opt/2, opt/3]).

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
-spec get_env(App::atom(), Par::atom(), Default::any()) -> {ok, Value::any()}.
get_env(App, Par, Default) -> 
    case application:get_env(App, Par) of
        {ok, Value} -> {ok, Value};
        _ ->           {ok, Default}
    end.


%% remote_start/2
%% ====================================================================
%% @doc start application on an (already running) remote node
%%
-spec remote_start(Node::atom(), Application::atom()) -> ok | {error, Reason::term()}.
remote_start(Node, Application) -> remote_start(Node, Application, temporary).

%% remote_start/3
%% ====================================================================
%% @doc start application on an (already running) remote node
%%
-spec remote_start(Node::atom(), Application::atom(), Type::restart_type()) -> ok | {error, Reason::term()}.
remote_start(Node, Application, Type) -> 
   rpc:block_call(Node, application, start, [Application, Type]).

%% remote_load/2
%% ====================================================================
%% @doc load application on an (already running) remote node.
%%
%% Beyond the node, see application:load for argument details.
%% @end
-spec remote_load(Node::atom(), Application::term()) -> ok | {error, Reason::term()}.
remote_load(Node, Application) -> rpc:block_call(Node, application, load, [Application]).

%% remote_set_env/4
%% ====================================================================
%% @doc set env values for a remote  application on an (already running) remote node
%%
-spec remote_set_env(Node::atom(), Application::atom(), Key::atom(), Val::term()) -> 
          ok | {error, Reason::term()}.
remote_set_env(Node, Application, Key, Val) ->
   rpc:block_call(Node, application, set_env, [Application, Key, Val]).

%% remote_set_env/5
%% ====================================================================
%% @doc set env values for a remote  application on an (already running) remote node
%% @end
-spec remote_set_env(Node::atom(), Application::atom(), Key::atom(), Val::term(), 
                     Timeout::pos_integer()) -> 
          ok | {error, Reason::term()}.
remote_set_env(Node, Application, Key, Val, Timeout) ->
   rpc:block_call(Node, application, set_env, [Application, Key, Val, Timeout]).

%% opt/3
%% ====================================================================
%% @doc retrieve a named option from an option list.
%%
%% <p>The options must be given as list of name-value pairs. If only the name
%% is given, it may serve as flag for a boolean value.</p>
%%
%% @param Name name of the option to select.
%% @param Options list of options.
%% @param Default default value of the option.
%% @returns the value of the option.
%% @end
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
-spec opt(Option::atom(), OptionList::[{atom(), term()} | atom()]) -> term().
opt(Op, Options) -> opt(Op, Options, false). 


%% set_opts/2
%% ----------------------------------------------------------------------
%% @doc set list of options in option list, overwriting existing values
%% @end
-spec set_opts(NewList :: [{atom(), term()}], Opts :: [{atom(), term()}]) -> 
          NewOpts :: [{atom(), term()}].
%% ----------------------------------------------------------------------
set_opts(NewList, Opts) ->
    AllOpts = Opts ++ NewList,
    

%% ====================================================================
%% Internal functions
%% ====================================================================


