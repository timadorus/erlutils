%% @author sage
%% @doc @todo Add description to teu_simple_tree_imp.


-module(teu_simple_tree_impl).

-behaviour(teu_tree).

-include_lib("eunit/include/eunit.hrl").

-record(node, { val = nil                :: term()
              , children =  array:new()  ::  array:array()
              }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, equal/2, add_child/3, delete_child/2, get_child/2, child_list/1,
         get_data/1, set_data/2, node_count/1]).


%% new/1
%% --------------------------------------------------------------------
-spec new(Args :: list()) -> Tree :: term().
%% --------------------------------------------------------------------
new(_Args) -> #node{}.


%% equal/2
%% --------------------------------------------------------------------
-spec equal(Tree1 :: term(), Tree2 :: term()) -> true | false.
%% --------------------------------------------------------------------
equal(Tree1, Tree2) when Tree1#node.val == Tree2#node.val ->
    Zip = teu_lists:zipfill(array:to_list(Tree1#node.children),
                            array:to_list(Tree2#node.children),
                            array:default(Tree1#node.children)),
    lists:all(fun({C1, C2}) -> equal(C1, C2) end , Zip);

%% handle the padded or deleted child cases
equal(undefined, undefined) -> true;

equal(_, _) -> false.

%% add_child/3
%% --------------------------------------------------------------------
-spec add_child(Parent :: term(), Child :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------
add_child(_Parent, Child, []) -> Child;

add_child(undefined, Child, Index) ->
    Parent = new([]),
    add_child(Parent, Child, Index);

add_child(Parent, Child, [Idx | Rest]) ->
    Children = Parent#node.children,
    NewChild = add_child(get_child(Parent, Idx), Child, Rest),
    Parent#node{children = array:set(Idx - 1, NewChild, Children)};

add_child(Parent, Child, Index) when is_integer(Index) ->
    Children = Parent#node.children,
    Parent#node{children = array:set(Index - 1, Child, Children)}.


%% delete_child/3
%% --------------------------------------------------------------------
-spec delete_child(Parent :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------
delete_child(Parent, Index) ->
    Children = Parent#node.children,
    Parent#node{children = array:set(Index - 1, array:default(Children), Children)}.


%% get_child/2
%% @doc return child node.
%% implements @see teu_tree:get_child/2.
%% --------------------------------------------------------------------
-spec get_child(Parent :: term(),
                Index :: non_neg_integer() | [non_neg_integer()]) ->
          Child :: term() | undefined.
%% --------------------------------------------------------------------

get_child(Parent, []) -> Parent;

get_child(Parent, [Idx | Rest]) -> get_child(get_child(Parent, Idx), Rest);

get_child(undefined, _Index) -> throw(bad_args);

get_child(Parent, Index) when is_integer(Index)-> array:get(Index - 1 , Parent#node.children).



%% child_list/2
%% --------------------------------------------------------------------
-spec child_list(Parent :: term()) -> Children :: [term()].
%% --------------------------------------------------------------------
child_list(Parent) -> array:to_list(Parent#node.children).


%% get_data/1
%% --------------------------------------------------------------------
-spec get_data(Tree :: term()) -> Data ::term().
%% --------------------------------------------------------------------
get_data(Tree) -> Tree#node.val.

%% set_data/2
%% --------------------------------------------------------------------
-spec set_data(Tree :: term(), Data :: term()) -> Tree ::term().
%% --------------------------------------------------------------------
set_data(Tree, Data) -> Tree#node{val = Data}.


%% node_count/1
%% --------------------------------------------------------------------
-spec node_count(Tree :: term()) -> pos_integer().
%% --------------------------------------------------------------------
node_count(#node{children = Children}) ->
    array:foldl(fun(_Idx, _Child, Sum) -> Sum+1 end, 1, Children).


%% ====================================================================
%% Internal functions
%% ====================================================================


