%% @author sage
%% @doc Tests teu_proces.


-module(teu_procs_tests).

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

%% info_test_() ->
%%     { setup, fun() -> ok end,
%%       fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

internals_test_() ->
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
      fun(_Args) -> [ ?_test(test_wait_for_exit())
                    , ?_test(test_wait_for_specific_exit())
                    , ?_test(test_wait_for_event_success())
                    , ?_test(test_wait_for_event_timeout())
                    , ?_test(test_wait_for_event_timeout_variable())
                    , ?_test(test_check_linked())
                   ]
      end }.

%% should waiting fail: eunit has a 15 sec standard  timeout
test_wait_for_exit() ->

	{ok, MPid} = teu_async_mock:start([]),

	teu_async_mock:stop(MPid),

	teu_procs:wait_for_exit(MPid),

	ok.


test_wait_for_specific_exit() ->

	{ok, MPid} = teu_async_mock:start([]),

	teu_async_mock:stop(MPid),

	teu_procs:wait_for_exit(MPid,normal),

	ok.

test_wait_for_event_success() ->

    {ok, MgrPid} = gen_event:start_link(),
    Event = {an_event, [foo, bar]},

    spawn(fun() -> timer:apply_after(100, gen_event, notify, [MgrPid, Event]) end),

    ?assertEqual({ok, Event}, teu_procs:wait_for_event(MgrPid, Event)),

    ok.


test_wait_for_event_timeout() ->

    {ok, MgrPid} = gen_event:start_link(),
    Event = {an_event, [foo, bar]},

    ?assertEqual({error, timeout}, teu_procs:wait_for_event(MgrPid, Event)),

    ok.

test_wait_for_event_timeout_variable() ->

    {ok, MgrPid} = gen_event:start_link(),
    Event = {an_event, [foo, bar]},

    ?assertEqual({error, timeout},
                 teu_procs:wait_for_event(MgrPid, Event, 1000)),

    ok.

test_check_linked() ->

    Pid = spawn_link(fun() -> timer:sleep(100000) end),

    ?assertEqual(true, teu_procs:check_linked(self(), Pid)),
    ?assertEqual(true, teu_procs:check_linked(Pid, self())),
    ?assertEqual(false, teu_procs:check_linked(self(), self())),

    ?assertEqual(false, teu_procs:check_linked(self(), a)),
    ?assertEqual(false, teu_procs:check_linked(a, self())),
    ?assertEqual(false, teu_procs:check_linked(a, b)),

    ok.