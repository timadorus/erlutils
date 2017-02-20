%% @author sage
%% @doc @todo Add description to teu_lists.


-module(teu_lists).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([kvlist_equal/2, keylist_equal/3, list_equal/2, contains_message/2,
         mapwhile/2, zipfill/3, listlen_equal/3]).

-ifdef(TEST).
-export([match_message/2]).
-endif.

%% kvlist_equal/2
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% @doc return true if two lists have the same lenght and each element is contained
%% in both lists, return false otherwise.
%%
%% @end
-spec kvlist_equal(KVList1::[{atom(), term()}], KVList2::[{atom(), term()}]) -> boolean().
%% --------------------------------------------------------------------
kvlist_equal(KVList1, KVList2) ->
    keylist_equal(1, KVList1, KVList2).

%% keylist_equal/3
%% --------------------------------------------------------------------
%% @doc return true if two lists have the same lenght and each element is contained
%% in both lists, return false otherwise.
%%
%% the key for sorting is to be found in position N of the tuple, starting at 1.
%% @end
-spec keylist_equal(N::pos_integer(), Keylist1::[tuple()], Keylist2::[tuple()]) -> boolean().
%% --------------------------------------------------------------------
keylist_equal(N, Keylist1, Keylist2) ->
    Sorted1 = lists:keysort(N, Keylist1),
    Sorted2 = lists:keysort(N, Keylist2),
    Result = lists:ukeymerge(N, Sorted1, Sorted2),
    listlen_equal(Keylist1, Keylist2, Result).

%% list_equal/2
%% --------------------------------------------------------------------
%% @doc return true if two lists have the same lenght and each element is contained
%% in both lists, return false otherwise.
%%
%% @end
-spec list_equal(L1 :: list(), L2 :: list()) -> boolean().
%% --------------------------------------------------------------------
list_equal(List1, List2) ->
    Sorted1 =lists:sort(List1),
    Sorted2 =lists:sort(List2),
    Result = lists:umerge(Sorted1, Sorted2),
    listlen_equal(List1, List2, Result).


%% listlen_equal/3
%% --------------------------------------------------------------------
%% @doc will return true if and only if all three lists are of the same length
%% @end
-spec listlen_equal(L1 :: list(), L1 :: list(), L1 :: list()) -> true | false.
%% --------------------------------------------------------------------
listlen_equal(L1, L2, L3) ->
    (length(L1) == length(L2)) and
        (length(L2) == length(L3)).

%% contains_message/2
%% ------------------------------------------------------------------
%% @doc return true if a matching message is element of the message list,
%% false otherwise.
%%
%% MsgList is list of arbitrary tuples or single atoms.
%% MsgPattern is single tuple whose elements are either specific values,
%% or functions of the pattern: fun(V) -> true| false. Or it may be single atom
%% or a single function of above format.
%% @end
-spec contains_message(MsgPattern, MsgList) ->  boolean() when
                      MsgPattern :: tuple() | term() | function(),
                      MsgList :: [tuple() | term()].
%% ------------------------------------------------------------------
contains_message(MsgPattern, MsgList)  ->
    lists:any(fun(Message) ->
                      match_message(MsgPattern, Message)
              end, MsgList).


%% mapwhile/2
%% ------------------------------------------------------------------
%% @doc apply map to elements of list until the map function returns false.
%% @todo test me
%% @param function to process elements
%% @param input list to be processed
%% @returns list of map results.
%% @end
-spec mapwhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: A) -> {true, B} | false),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().
%% ------------------------------------------------------------------
mapwhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
    {true, Val} -> [Val|mapwhile(Pred, Tail)];
    false -> []
    end;
mapwhile(Pred, []) when is_function(Pred, 1) -> [].


%% zipfill/3
%% ------------------------------------------------------------------
%% @doc zip two lists.
%% works like @see lists:zip/2, but will fill the missing value with
%% given padding, should the lists be of unequal length
%% @param List1
%% @param List2
%% @param Pad
-spec zipfill(L1 :: list(), L2 :: list(), Pad :: any()) -> list().
%% ------------------------------------------------------------------
zipfill([X | Xs], [Y | Ys], P) -> [{X, Y} | zipfill(Xs, Ys, P)];

zipfill([X | Xs], [], P) -> [{X, P} | zipfill(Xs, [], P)];

zipfill([], [Y | Ys], P) -> [{P, Y} | zipfill([], Ys, P)];

zipfill([], [], _) -> [].


%% ====================================================================
%% Internal functions
%% ====================================================================

match_message(Pattern, Message)
  when is_tuple(Pattern) and is_tuple(Message) ->
    match_message(tuple_to_list(Pattern),
                  tuple_to_list(Message));

match_message(Pattern, Message)
  when is_atom(Pattern) ->
    Pattern =:= Message;

match_message(Pattern, Message)
  when is_function(Pattern, 1) ->
    Pattern(Message);

match_message([FirstPat|PatternRest], [FirstMsg|MessageRest])
  when is_function(FirstPat, 1) ->
    case FirstPat(FirstMsg) of
        false -> false;
        true -> match_message(PatternRest, MessageRest)
    end;

match_message([FirstPat|PatternRest], [FirstMsg|MessageRest]) ->
    case FirstPat =:= FirstMsg of
         false -> false;
   true -> match_message(PatternRest, MessageRest)
    end;

match_message([], []) ->
    true;

%% everything i don't understand should be false
match_message(_, _) -> false.


