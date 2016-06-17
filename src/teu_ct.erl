%% @author sage
%% @doc @todo Add description to teu_ct.


-module(teu_ct).

%% ====================================================================
%% API functions
%% ====================================================================
-export([config/2]).


config([F|R], Config) when R /= [] ->
    case lists:keysearch(F,1,Config) of
    {value,{F,Val}} ->
        config(R, Val);
    _ ->
        io:format("Could not find element ~p in Config.~n",[F]),
        undefined
    end;

config([Rest], Config) -> config(Rest, Config);

config(Key, Config) ->
    case lists:keysearch(Key,1,Config) of
    {value,{Key,Val}} ->
        Val;
    _ ->
        io:format("Could not find element ~p in Config.~n",[Key]),
        undefined
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


