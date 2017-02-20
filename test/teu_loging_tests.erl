%%
%% @doc TODO: Add description to teu_loggin_tests
%%
%% @author sage
%%
%% @copyright 2009-2013 Timadorus Project

-module(teu_loging_tests).

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
                    ?_test(test_basic())
                   ]
      end }.



%%
%% Local Functions
%%
test_basic() ->
    teu_loging:log_protocol_msg(self(), ?MODULE, this_is_a_test),
  ok.

