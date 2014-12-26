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
      fun(_Args) -> [
                    ?_test(test_wait_for_exit()),
                    ?_test(test_wait_for_specific_exit())
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