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


internals_test_() ->
    { "test internal functions",
      setup,

      fun() ->
              ok
      end,

      fun(_Args) ->
              ok
      end,
      fun(_Args) -> [ ?_test(test_keylist_equal())
                    , ?_test(test_kvlist_equal())
                    , ?_test(test_list_equal())
                    , ?_test(test_match_message())
                    , ?_test(test_zipfill())
                    , ?_test(test_listlen_equal3())
                   ]
      end }.


test_listlen_equal3() ->
    ?assertEqual(true, teu_lists:listlen_equal([], [], [])),

    ?assertEqual(false, teu_lists:listlen_equal([1], [], [])),
    ?assertEqual(false, teu_lists:listlen_equal([], [1], [])),
    ?assertEqual(false, teu_lists:listlen_equal([], [], [1])),

    ?assertEqual(false, teu_lists:listlen_equal([1], [1], [])),
    ?assertEqual(false, teu_lists:listlen_equal([], [1], [1])),
    ?assertEqual(false, teu_lists:listlen_equal([1], [], [1])),

    ?assertEqual(false, teu_lists:listlen_equal([1, 2], [1, 1], [1])),
    ?assertEqual(false, teu_lists:listlen_equal([1, 2], [1], [1, 1])),
    ?assertEqual(false, teu_lists:listlen_equal([1], [1, 2], [1, 1])),

    ?assertEqual(true, teu_lists:listlen_equal([1], [1], [1])),
    ?assertEqual(true, teu_lists:listlen_equal([2], [3], [4])),
    ?assertEqual(true, teu_lists:listlen_equal([1], [zwei], [3.0])),

    ok.

test_match_message() ->
    TestCases = [{atom, atom, true},
                 {atom, other_atom, false},
                 {fun is_atom/1, atom, true},
                 {fun is_list/1, atom, false},
                 {{}, {}, true},
                 {{atom}, {atom}, true},
                 {{other_atom}, {atom}, false},
                 {{1}, {atom}, false},
                 {{1}, {1}, true},
                 {{fun is_atom/1}, {atom}, true},
                 {{fun is_list/1}, {atom}, false},
                 {{one, fun is_atom/1, three}, {one, two, three}, true},
                 {{one, fun is_atom/1, three}, {one, two, four}, false},
                 {{one, fun is_number/1, three}, {one, 2, three}, true},
                 {{one, fun is_atom/1, three}, {one, 2, three}, false}
                 ],

    lists:foreach(fun({Pattern, Message, Expected}) ->
%%                           ?debugFmt("Pattern: ~p, Message: ~p, Expected:~p~n",[Pattern, Message, Expected]),
                          ?assertEqual(Expected,
                                       teu_lists:match_message(Pattern, Message))
                   end, TestCases),

    ok.

%% verify keylist comparison
test_keylist_equal() ->

    Keylist1 = [{a, 1}, {b, 2}, {c, 3}],

    Keylist2 = [{a, 1}, {b, 2}, {c, 3}],
    Keylist3 = [{a, 1}, {b, 2}],
    Keylist4 = [{a, 1}, {b, 2}, {b, zwo}],
    Keylist5 = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],

    ?assertEqual(true, teu_lists:keylist_equal(2, [], [])),

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

    ?assertEqual(true, teu_lists:kvlist_equal([], [])),

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

    ?assertEqual(true, teu_lists:list_equal([], [])),

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

test_zipfill() ->
   ?assertEqual([], teu_lists:zipfill([], [], a)),
   ?assertEqual([{1, 2}], teu_lists:zipfill([1], [2], a)),
   ?assertEqual([{1, 2}, {3, 4}], teu_lists:zipfill([1, 3], [2, 4], a)),

    OutList1 = [{1, 4}, {2, 5}, {3, a}],
    ?assertEqual(OutList1, teu_lists:zipfill([1, 2, 3], [4, 5], a)),

    OutList2 = [{1, 8}, {2, 9}, {b, 0}],
    ?assertEqual(OutList2, teu_lists:zipfill([1, 2], [8, 9, 0], b)),

    ok.
