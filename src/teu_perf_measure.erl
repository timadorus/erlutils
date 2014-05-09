%% @author sage
%% @doc tools and helper functions to measure the performance of functions 
%% and modules.
%%
%% percentile calculation by rodo. See: https://gist.github.com/rodo/5495391



-module(teu_perf_measure).

%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("eunit/include/eunit.hrl").

-export([percentile/2, percentile95/1,percentile98/1]).
-export([test_avg/5, test_avg_func/7, test_avg_list/5]).

%% test_avg/5
%% ----------------------------------------------------------------------------
%% @doc run function N number of times, 
%% timing each invocation and computing minimum, maximum, Median and average 
%% for all runs.
%%
%% function returns median of all tests.
%% @end
-spec test_avg(Title::string(), Module :: atom(), Function :: atom(), 
               Args :: list(), N :: pos_integer()) -> float(). 
%% ----------------------------------------------------------------------------
test_avg(T, M, F, A, N) when N > 0 ->
    L = test_loop(M,F,A,N,[]),
    test_calc(T,L).


%% test_avg_func/5
%% ----------------------------------------------------------------------------
%% @doc run function N number of times, calling a generator each time. 
%% timing each invocation and computing minimum, maximum, Median and average 
%% for all runs.
%%
%% generator is a function of type:  f(A) -> B
%%
%% the result of the generator will be prependet to the argument list of 
%% the next invocation and used as input for the next call to the generator.
%%
%% function returns median of all tests.
%% @end
-spec test_avg_func(Title::string(), Module :: atom(), Function :: atom(), 
                    Args :: list(), GenInput :: term(), Generator :: function(), 
                    N :: pos_integer()) -> float(). 
%% ----------------------------------------------------------------------------
test_avg_func(T,M,F,A,L,G,N) when N > 0 ->
    ResList = test_loop_func(M, F, A, L, G, N, []),
    test_calc(T, ResList).

%% test_avg_list/5
%% ----------------------------------------------------------------------------
%% @doc run function with args, with one element of argslist for each invocation,
%% timing each invocation and computing minimum, maximum, Median and average 
%% for all runs.
%%
%% function returns median of all tests.
%% @end
-spec test_avg_list(Title::string(), Module :: atom(), Function :: atom(), 
                    Args :: list(), ArgList :: list()) -> float(). 
%% ----------------------------------------------------------------------------
test_avg_list(Title,Module,Function,Args,ArgList) when length(ArgList) > 0  ->
  ResList = test_loop_list(Module, Function, Args, ArgList, []),
  test_calc(Title,ResList).


%% percentile95/1
%% ----------------------------------------------------------------------
%% @doc return the 95th percentile of the list 
%% @end
-spec percentile95(L::[float()|integer()]) -> float.
%% ----------------------------------------------------------------------
percentile95(L)-> percentile(L,0.95).


%% percentile98/1
%% ----------------------------------------------------------------------
%% @doc return the 98th percentile of the list 
%% @end
-spec percentile98(L::[float()|integer()]) -> float.
%% ----------------------------------------------------------------------
percentile98(L)-> percentile(L,0.98).


%% percentile/2
%% ----------------------------------------------------------------------
%% @doc return the Nth percentile of the list
%%
%% L: list of values
%% P: nth percentile as percentage (0.0 < P < 1.0)
%% 
%% @end
-spec percentile(L::[number()], P::float()) -> float.
%% ----------------------------------------------------------------------
percentile(L, P)->
    K=(length(L) - 1) * P,
    F=floor(K),
    C=ceiling(K),
    final(lists:sort(L),F,C,K).


%% ====================================================================
%% Percentile Calculations
%% ====================================================================

final(L,F,C,K) when (F == C)->
    lists:nth(trunc(K)+1,L);
final(L,F,C,K) ->
    pos(L,F,C,K) + pos(L,C,K,F).
 
 
pos(L,A,B,C)->
    lists:nth(trunc(A)+1,L) * (B-C).

%% @doc http://schemecookbook.org/Erlang/NumberRounding
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
            T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%% @doc http://schemecookbook.org/Erlang/NumberRounding
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
            T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% ====================================================================
%% Performance Tests
%% ====================================================================

test_calc(Title, L) -> 
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    P10 = percentile(L, 0.1),
    P90 = percentile(L, 0.9),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Performance for ~p",[Title]),
    io:format("Range: ~b - ~b mics~n"
              "P10: ~b, P90: ~b"
              "Median: ~b mics~n"
              "Average: ~b mics~n",
                      [Min, Max, P10, P90, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, [N|A]),
    ?debugFmt("~p: ~p mics",[N,T]),
    test_loop(M, F, A, N - 1, [T|List]).

test_loop_func(_M, _F, _A, _L, _G, 0, List) ->
    List;
test_loop_func(M, F, A, L, G, N, List) ->
    FrontArg = erlang:apply(G, [L]),
    {T, _Result} = timer:tc(M, F, [FrontArg|A]),
    test_loop_func(M, F, A, FrontArg, G, N - 1, [T|List]).

test_loop_list(_M, _F, _A, [], List) ->
  List;
test_loop_list(M, F, A, AL, List) ->
  [FrontArg|ArgsRest] = AL,
  {T, _Result} = timer:tc(M, F, [FrontArg|A]),
  test_loop_list(M, F, A, ArgsRest, [T|List]).

