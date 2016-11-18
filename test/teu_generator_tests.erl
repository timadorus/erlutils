%%
%% @doc TODO: Add description to teu_loggin_tests
%%
%% @author sage
%%
%% @copyright 2009-2013 Timadorus Project

-module(teu_generator_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% Fixtures
%%

initial_test_() ->
    { "some initial tests",
      setup,

      fun() ->
              %%     application:start(sasl),
              ok
      end,

      fun(_Args) ->
              %%     application:stop(sasl),
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_start_link())
                   ]
      end }.



%%
%% Local Functions
%%

test_start_link() ->
    {ok, Pid} = teu_generator:start_link(teu_test_generator, [], []),
    
    ?assertMatch(P when is_pid(P), Pid),

ok.

