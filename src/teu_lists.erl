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
-export([kvlist_equal/2, keylist_equal/3, list_equal/2, contains_message/2]).

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
-spec kvlist_equal(KVList1::[{atom, term()}], KVList2::[{atom, term()}]) -> true | false.
%% --------------------------------------------------------------------
kvlist_equal(KVList1, KVList2) ->
	keylist_equal(1,KVList1, KVList2).
  
%% keylist_equal/3
%% --------------------------------------------------------------------
%% @doc return true if two lists have the same lenght and each element is contained
%% in both lists, return false otherwise.
%%
%% @end
-spec keylist_equal(N::pos_integer(), Keylist1::[tuple()], Keylist2::[{tuple()}]) -> true | false.
%% --------------------------------------------------------------------
keylist_equal(N,Keylist1, Keylist2) ->
	Sorted1 = lists:keysort(N, Keylist1),
	Sorted2 = lists:keysort(N, Keylist2),
	Result = lists:ukeymerge(N, Sorted1, Sorted2),
	(length(Keylist1) == length(Result)) and
		(length(Keylist2) == length(Result)).


%% list_equal/2
%% --------------------------------------------------------------------
%% @doc return true if two lists have the same lenght and each element is contained
%% in both lists, return false otherwise.
%%
%% @end
-spec list_equal(L1 :: list(), L2 :: list()) -> true|false.
%% --------------------------------------------------------------------
list_equal(List1, List2) ->
	Sorted1 =lists:sort(List1),
	Sorted2 =lists:sort(List2),
	Result = lists:umerge(Sorted1, Sorted2),
	(length(List1) == length(Result)) and 
		(length(List2) == length(Result)).

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
-spec contains_message(MsgPattern, MsgList) ->  true  | false when
                      MsgPattern :: tuple() | term() | function(), 
                      MsgList :: [tuple() | term()].
%% ------------------------------------------------------------------
contains_message(MsgPattern, MsgList)  ->
	lists:any(fun(Message) ->
					  match_message(MsgPattern, Message)
			  end, MsgList).


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
  when is_function(Pattern,1) ->
	Pattern(Message);

match_message([FirstPat|PatternRest], [FirstMsg|MessageRest])
  when is_function(FirstPat, 1) -> 
	case FirstPat(FirstMsg) of
		false -> false;
		true -> match_message(PatternRest,MessageRest)
	end;

match_message([FirstPat|PatternRest], [FirstMsg|MessageRest]) ->
	case FirstPat =:= FirstMsg of
		 false -> false;
   true -> match_message(PatternRest,MessageRest)
	end;

match_message([], []) ->
	true;

%% everything i don't understand should be false
match_message(_, _) -> false.