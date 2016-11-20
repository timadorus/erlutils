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

start_stop_test_() ->
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
      fun(_Foo) -> [ ?_test(test_start_link_stop())
                   ]
      end }.

api_test_() ->
    { "test API",
      setup,

      fun() ->
              %%     application:start(sasl),

              {ok, Pid} = teu_generator:start_link(teu_test_generator, [], []),
              Pid
      end,
      
      fun(Pid) ->
              unlink(Pid),
              teu_generator:stop(Pid),
              teu_procs:wait_for_exit(Pid, normal),
              
              %%     application:stop(sasl),
              ok
      end,
      fun(Args) -> [ ?_test(test_get_work(Args))
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

test_get_work(_CtrlPid) ->
%%     teu_generator:get_work(CtrlPid, GenRef), 
    ok.

test_make_gen() ->
    Module = teu_test_generator,
    GenStartArgs = [foo, bar],
    CtrlState = [alpha, omega],
    State = #state{module = Module, 
                   gen_start_args = GenStartArgs,
                   ctrl_state = CtrlState},
    WorkRef = make_ref(),
    WorkSpec = the_work_you_do,
    NewState = State#state{gen_pids = sets:add_element(self(), State#state.gen_pids)},

    M = em:new(),
    em:strict(M, teu_generator_wrk, start_link, 
              [Module, []], 
              {return, {ok, self()}}),
    em:strict(M, teu_test_generator, make_work, 
              [CtrlState], 
              {return, {ok, WorkRef, WorkSpec, CtrlState}}),
    em:strict(M, teu_generator_wrk, do_work, 
              [self(), WorkRef, WorkSpec], 
              {return, {ok, self()}}),
    em:replay(M),

    ?assertEqual(NewState, teu_generator:make_gen(State)),
    
    em:verify(M),
    ok.