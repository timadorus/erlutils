%% @author sage
%% @doc @todo Add description to teu_simple_tree_imp.


-module(teu_simple_tree_imp).

-behaviour(teu_tree).

-include_lib("eunit/include/eunit.hrl").

-record(node, { key = nil    :: term()
              , val = nil    :: term()
              , children =[] :: [term()]
              }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, add_child/, delete_child/3, get_child/2, child_list/2,
         get_data/1, set_data/2]).


%% new/1
%% --------------------------------------------------------------------
-spec new(Args :: list()) -> Tree :: term().
%% --------------------------------------------------------------------
new(_Args) -> #node{}

%% add_child/3
%% --------------------------------------------------------------------
-spec add_child(Parent :: term(), Child :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------
add_child(Parent, Child, Index) ->
    Children = Parent#node.children,
    NewList = do_add_child(Children, length(Children), Index, Child),
    Parent#node{children = NewList}.


do_add_child(Children,CLen, Index, Child) when CLen < Index ->
    Padding = lists:duplicate(Index - CLen, nil),
    Children ++ Padding ++ [Index];

do_add_child(Childre,CLen, Index, Child) when CLen == Index ->
    

%% delete_child/3
%% --------------------------------------------------------------------
-spec delete_child(Parent :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------

%% get_child/2
%% @od this may allow a more optimized implementation, otherwise this
%% would be:
%% <pre>
%%  get_child(P,I) -> lists:nth(I,tree_impl:get_list(P).
%% </pre>
%% --------------------------------------------------------------------
-spec get_child(Parent :: term(), Index :: non_neg_integer()) -> Child :: term().

%% --------------------------------------------------------------------

%% child_list/2
%% --------------------------------------------------------------------
-spec add_child(Parent :: term()) -> Children :: [term()].
%% --------------------------------------------------------------------


%% get_data/1
%% --------------------------------------------------------------------
-spec get_data(Tree :: term()) -> Data ::term().
%% --------------------------------------------------------------------

%% set_data/2
%% --------------------------------------------------------------------
-spec set_data(Tree :: term(), Data :: term()) -> Tree ::term().
%% --------------------------------------------------------------------


%% ====================================================================
%% Internal functions
%% ====================================================================


