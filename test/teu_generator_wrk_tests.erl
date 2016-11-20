%%
%% @doc Test the generator worker.
%%
%% @author sage
%%
%% @copyright 2009-2016 Timadorus Project

-module(teu_generator_wrk_tests).

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

api_test_() ->
    { "API tests",
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
                    ?_test(test_start_link_stop())
                   ]
      end }.



%%
%% Local Functions
%%

test_start_link_stop() ->
    {ok, Pid} = teu_generator_wrk:start_link(teu_test_generator, []),
    
    ?assertMatch(P when is_pid(P), Pid),

    ?assertEqual(true, erlang:is_process_alive(Pid)),

    unlink(Pid),
    teu_generator:stop(Pid),
    teu_procs:wait_for_exit(Pid, normal),

ok.

