%%
%% @doc Test the generator
%%
%% @author sage
%%
%% @copyright 2009-2016 Timadorus Project

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

api_test_() ->
    { "test API",
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

internal_test_() ->
    { "test internal functions",
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
                    ?_test(test_make_gen())
                   ]
      end }.


-record(state, { module                  :: atom()
               , ctrl_state              :: term()
               , gen_pids = sets:new()   :: sets:set()
               , gen_start_args          :: list()   %% starting argument for new generators
               }).

%%
%% Local Functions
%%

test_start_link_stop() ->
    {ok, Pid} = teu_generator:start_link(teu_test_generator, [], []),
    
    ?assertMatch(P when is_pid(P), Pid),

    ?assertEqual(true, erlang:is_process_alive(Pid)),

    unlink(Pid),
    teu_generator:stop(Pid),
    teu_procs:wait_for_exit(Pid, normal),

ok.


test_make_gen() ->
    Module = teu_test_generator,
    GenStartArgs = [foo, bar],
    State = #state{module = Module, gen_start_args = GenStartArgs},
    NewState = State#state{gen_pids = sets:add_element(self(), State#state.gen_pids)},

    M = em:new(),
    em:strict(M, teu_generator_wrk, start_link, 
              [Module, GenStartArgs, self(), []], 
              {return, {ok, self()}}),
    em:replay(M),

    ?assertEqual(NewState, teu_generator:make_gen(State)),
    
    em:verify(M),
    ok.