%%
%% @doc TODO: Add description to teu_logging
%%
%% @author sage
%%
%% @copyright 2009-2013 Timadorus Project

-module(teu_loging).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([log_protocol_msg/3]).

%%
%% API Functions
%%

%% log_protocol_msg/3
%% --------------------------------------------------------------------
%% @doc send a protocol message to proper log.
%% @end
%% --------------------------------------------------------------------
-spec log_protocol_msg(PID::pid(), Module::atom(), Msg::term()) -> ok.
log_protocol_msg(PID, Module, Msg) ->
    
    %% for now, just write to stdout
    io:fwrite("~s:~w: MSG: ~p~n", [Module, PID, Msg]),
    ok.


%%
%% Local Functions
%%

