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
         get_data/1, set_data/2]).


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
    CL1 = array:to_list(Tree1#node.children),
    CL2 = array:to_list(Tree2#node.children),
  case length(CL1) == length(CL2) of
      false -> false;
      true -> lists:all(fun({C1, C2}) -> equal(C1, C2) end , lists:zip(CL1, CL2))
  end;

equal(_,_) -> false.

%% add_child/3
%% --------------------------------------------------------------------
-spec add_child(Parent :: term(), Child :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------
add_child(Parent, Child, Index) ->
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
%% @od this may allow a more optimized implementation, otherwise this
%% would be:
%% <pre>
%%  get_child(P,I) -> lists:nth(I,tree_impl:get_list(P).
%% </pre>
%% --------------------------------------------------------------------
-spec get_child(Parent :: term(), Index :: non_neg_integer()) -> Child :: term() | undefined.
%% --------------------------------------------------------------------
get_child(Parent, Index) -> array:get(Index - 1 , Parent#node.children).

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


%% ====================================================================
%% Internal functions
%% ====================================================================


