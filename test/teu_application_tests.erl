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

%% info_test_() ->
%%     { setup, fun() -> ok end,
%%       fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

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

    application:set_env(teu_app_test, par1, 2),
	?assertEqual({ok, 2}, teu_application:get_env(teu_app_test, par1, 3)),
    
	?assertEqual({ok, 3}, teu_application:get_env(teu_app_test, par2, 3)),
    
    ok.

-spec test_opt() -> ok.
test_opt() ->
    
%%    ?debugMsg("starting opt"),
    
    Options = [register, {config, {1,2,3, [foo, bar]}}],
    
    ?assertEqual(true, teu_application:opt(register, Options, false)),

    ?assertEqual({1,2,3, [foo, bar]}, teu_application:opt(config, Options, [])),

    ?assertEqual(no_sir, teu_application:opt(invalid, Options, no_sir)),
    
    ?assertEqual(false, teu_application:opt(invalid, Options)),

    ?assertEqual(true, teu_application:opt(register, Options)),

%%   ?debugHere,
  ok.

