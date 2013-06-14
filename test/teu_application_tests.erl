%%
%% @doc TODO: Add description to teu_nodes_tests
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_application_tests).

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
                    ?_test(test_read_config()),
                    ?_test(test_opt())
                   ]
      end }.



%%
%% Local Functions
%%
-spec test_read_config() ->ok.
test_read_config() ->
    %% Fixme: cannot test, since this process does not belong to any app
%%     application:set_env(App, test_dummy_par1, 2),
%% 	?assertEqual({ok, 2}, teu_application:get_env(test_dummy_par1, 3)),
%% 	?assertEqual({ok, 3}, teu_application:get_env(test_dummy_par2, 3)),

	?assertEqual({ok, 2}, teu_application:get_env(par1, teu_app_test, 3)),
	?assertEqual({ok, 3}, teu_application:get_env(par2, teu_app_test, 3)),
    
    ok.

-spec test_opt() -> ok.
test_opt() ->
    
%%  ?debugMsg("starting opt"),
    
    Options = [register, {config, {1,2,3, [foo, bar]}}],
    
    ?assertEqual(true, quperl_client:opt(register, Options, false)),

    ?assertEqual({1,2,3, [foo, bar]}, quperl_client:opt(config, Options, [])),

    ?assertEqual(no_sir, quperl_client:opt(invalid, Options, no_sir)),
    
    ?assertEqual(false, quperl_client:opt(invalid, Options)),

    ?assertEqual(true, quperl_client:opt(register, Options)),

%%  ?debugHere,
  ok.

