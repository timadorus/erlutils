%%
%% @doc TODO: Add description to teu_nodes_tests
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_async_mock_tests).

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

info_test_() ->
    { setup, fun() -> ok end,
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

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
                    ?_test(test_send_message()),
                    ?_test(test_register())
                   ]
      end }.



%%
%% Local Functions
%%
test_send_message() ->
	Res = teu_async_mock:start_link([]),
	?assertMatch({ok, _Pid}, Res),
	{ok, Pid} = Res,
	
	?assertMatch(undefined, whereis(true)),
	?assertMatch(undefined, whereis(teu_async_mock)),
	
	gen_server:cast(Pid, test_message),
	
	?assertEqual(test_message, teu_async_mock:last_message(Pid)),
	
    ok.

test_register() ->
	
    {ok, Pid1} = teu_async_mock:start_link([register]),

    ?assertEqual(Pid1, whereis(teu_async_mock)),

    teu_async_mock:stop(Pid1),
    sleep(100),
    ?assertEqual(undefined, whereis(teu_async_mock)),

    {ok, Pid2} = teu_async_mock:start_link([{register, my_proc_name}]),

    ?assertEqual(Pid2, whereis(my_proc_name)),

    teu_async_mock:stop(Pid2),
    sleep(100),
    ?assertEqual(undefined, whereis(my_proc_name)),

    ok.


%% sleep for number of miliseconds
sleep(T) ->
  receive 
    after T -> ok 
  end.
