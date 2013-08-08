%% @author sage
%% @doc @todo Add description to teu_lists_tests.


-module(teu_lists_tests).

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
                    ?_test(test_keylist_equal()),
                    ?_test(test_kvlist_equal()),
                    ?_test(test_list_equal())
                   ]
      end }.


%% verify keylist comparison
test_keylist_equal() ->
	
	Keylist1 = [{a, 1}, {b, 2}, {c, 3}],
	
	Keylist2 = [{a, 1}, {b, 2}, {c, 3}],
	Keylist3 = [{a, 1}, {b, 2}],
	Keylist4 = [{a, 1}, {b, 2}, {b, zwo}],
	Keylist5 = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],

    ?assertEqual(true, teu_lists:keylist_equal(2, [],[])),

	?assertEqual(true, teu_lists:keylist_equal(2, Keylist1, Keylist1)),
	
	?assertEqual(true, teu_lists:keylist_equal(2, Keylist1, Keylist2)),
	?assertEqual(true, teu_lists:keylist_equal(2, Keylist2, Keylist1)),

	?assertEqual(false, teu_lists:keylist_equal(2, Keylist1, Keylist3)),
	?assertEqual(false, teu_lists:keylist_equal(2, Keylist3, Keylist1)),

	?assertEqual(false, teu_lists:keylist_equal(2, Keylist1, Keylist4)),
	?assertEqual(false, teu_lists:keylist_equal(2, Keylist4, Keylist1)),

	?assertEqual(false, teu_lists:keylist_equal(2, Keylist1, Keylist5)),
	?assertEqual(false, teu_lists:keylist_equal(2, Keylist5, Keylist1)),

	ok.

%% verify keylist comparison
test_kvlist_equal() ->
	
	Keylist1 = [{a, 1}, {b, 2}, {c, 3}],
	
	Keylist2 = [{a, 1}, {b, 2}, {c, 3}],
	Keylist3 = [{a, 1}, {b, 2}],
	Keylist4 = [{a, 1}, {b, 2}, {b, zwo}],
	Keylist5 = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],

    ?assertEqual(true, teu_lists:kvlist_equal([],[])),

	?assertEqual(true, teu_lists:kvlist_equal(Keylist1, Keylist1)),
	
	?assertEqual(true, teu_lists:kvlist_equal(Keylist1, Keylist2)),
	?assertEqual(true, teu_lists:kvlist_equal(Keylist2, Keylist1)),

	?assertEqual(false, teu_lists:kvlist_equal(Keylist1, Keylist3)),
	?assertEqual(false, teu_lists:kvlist_equal(Keylist3, Keylist1)),

	?assertEqual(false, teu_lists:kvlist_equal(Keylist1, Keylist4)),
	?assertEqual(false, teu_lists:kvlist_equal(Keylist4, Keylist1)),

	?assertEqual(false, teu_lists:kvlist_equal(Keylist1, Keylist5)),
	?assertEqual(false, teu_lists:kvlist_equal(Keylist5, Keylist1)),

	ok.


%% verify list comparison
test_list_equal() ->
	
	Keylist1 = [a, b, c],
	
	Keylist2 = [a, b, c],
	Keylist3 = [a, b],
	Keylist4 = [a, b, b],
	Keylist5 = [a, b, c, d],

    ?assertEqual(true, teu_lists:list_equal([],[])),

	?assertEqual(true, teu_lists:list_equal(Keylist1, Keylist1)),
	
	?assertEqual(true, teu_lists:list_equal(Keylist1, Keylist2)),
	?assertEqual(true, teu_lists:list_equal(Keylist2, Keylist1)),

	?assertEqual(false, teu_lists:list_equal(Keylist1, Keylist3)),
	?assertEqual(false, teu_lists:list_equal(Keylist3, Keylist1)),

	?assertEqual(false, teu_lists:list_equal(Keylist1, Keylist4)),
	?assertEqual(false, teu_lists:list_equal(Keylist4, Keylist1)),

	?assertEqual(false, teu_lists:list_equal(Keylist1, Keylist5)),
	?assertEqual(false, teu_lists:list_equal(Keylist5, Keylist1)),

	ok.
