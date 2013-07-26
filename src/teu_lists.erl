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
-export([kvlist_equal/2, keylist_equal/3, list_equal/2]).

-spec kvlist_equal(KVList1::[{atom, term()}], KVList2::[{atom, term()}]) -> true | false.
kvlist_equal(KVList1, KVList2) ->
	keylist_equal(1,KVList1, KVList2).
  
%%
%% return true if both lists are equal
-spec keylist_equal(N::pos_integer(), Keylist1::[tuple()], Keylist2::[{tuple()}]) -> true | false.
keylist_equal(N,Keylist1, Keylist2) ->
	Sorted1 = lists:keysort(N, Keylist1),
	Sorted2 = lists:keysort(N, Keylist2),
	Result = lists:ukeymerge(N, Sorted1, Sorted2),
	(length(Keylist1) == length(Result)) and
		(length(Keylist2) == length(Result)).


list_equal(List1, List2) ->
	Sorted1 =lists:sort(List1),
	Sorted2 =lists:sort(List2),
	Result = lists:umerge(Sorted1, Sorted2),
	(length(List1) == length(Result)) and 
		(length(List2) == length(Result)).

%% ====================================================================
%% Internal functions
%% ====================================================================


