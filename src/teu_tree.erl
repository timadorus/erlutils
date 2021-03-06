%% @author sage
%% @doc define an behavoiour for a tree.
%%
%% Indices for children in a node with n children range from 1-n
%%
%% The implementation must provide the capability of an empty tree.
%% @end


-module(teu_tree).

%% new/0
%% --------------------------------------------------------------------
%% @ doc generate a new tree.
%% note: this function must not fail. There must be no situation in which
%% invalid combination of arguments will cause the creation of a tree to fail.
%% Only in case of the runtime system refusing to provide needed resource may
%% the code throw an appropriate exception/error.
%% @ end
-callback new(Args :: list()) -> Tree :: term().
%% --------------------------------------------------------------------

%% add_child/3
%% @doc add child in nth position.
%% @param Index index of the child to select or list of indicies to descibre
%%              path through tree.
%% @returns the tree created by the change. If the list of indices is empty,
%%          the child will be returned.
%% --------------------------------------------------------------------
-callback add_child(Parent :: term(), Child :: term(),
                    Index :: non_neg_integer() | [non_neg_integer()]) ->
             Tree :: term().
%% --------------------------------------------------------------------

%% delete_child/3
%% --------------------------------------------------------------------
-callback delete_child(Parent :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------

%% get_child/2
%% @doc retrieve the nth child of a node.
%% @param Index index of the child to select or list of indicies to describe
%%              path through tree.
%% @throws bad_args if the index is a list and any but the last of the
%%                  indicies indicate a non existing node
%% @returns the child indicated by the index. If the child does not exist,
%%          undefined is returned. If the list of indices is empty, the Parent
%%          will be returned.
%% --------------------------------------------------------------------
-callback get_child(Parent :: term(),
                    Index :: non_neg_integer() | [non_neg_integer()]) ->
             Child :: term() | undefined.

%% --------------------------------------------------------------------

%% child_list/2
%% --------------------------------------------------------------------
-callback child_list(Parent :: term()) -> Children :: [term()].
%% --------------------------------------------------------------------


%% equal/2
%% @doc compare two trees for equality.
%% the data withing the nodes is tested by equality ('==').
%% @end
%% --------------------------------------------------------------------
-callback equal(Tree1 :: term(), Tree2 :: term()) -> true | false.
%% --------------------------------------------------------------------

%% get_data/1
%% --------------------------------------------------------------------
-callback get_data(Tree :: term()) -> Data ::term().
%% --------------------------------------------------------------------

%% set_data/2
%% --------------------------------------------------------------------
-callback set_data(Tree :: term(), Data :: term()) -> Tree ::term().
%% --------------------------------------------------------------------

%% node_count/1
%% --------------------------------------------------------------------
-callback node_count(Tree :: term()) -> pos_integer().
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/2, equal/2, add_child/3, children/1, get_child/2,
         delete_child/2, set_data/2, get_data/1, node_count/1]).

-record(tree, { module :: atom()
              , root   :: term()
              }).

%% new/2
%% --------------------------------------------------------------------
%% @doc create a new tree.
%% @param Module the module that will implement the tree
%% @param Args any additional args to pass to the module for creating a new tree
%% @end
-spec new(Module :: atom, Args :: list()) -> Tree :: term().
%% --------------------------------------------------------------------
new(Module, Args) -> #tree{module = Module, root = Module:new(Args)}.


%% equal/2
%% --------------------------------------------------------------------
-spec equal(T1 :: term(), T2 :: term()) -> true | false.
%% --------------------------------------------------------------------
equal(#tree{module = Module, root = Root1}, #tree{module = Module, root = Root2}) ->
    Module:equal(Root1, Root2).

%% add_child/3
%% --------------------------------------------------------------------
%% @doc add a subtree into a tree.
%% any subtree currently set at position n will be overwritten.
%% @param Index 1 =< index =< n or list thereof.
%% @end
-spec add_child(Tree :: term(), Child :: term(),
                Index :: non_neg_integer() | [non_neg_integer()]) ->
          Tree :: term().
%% --------------------------------------------------------------------
add_child(#tree{ module = Module, root = Root},
    #tree{module = Module, root = Child}, Index) ->
    #tree{module = Module, root = Module:add_child(Root, Child, Index)};

add_child(#tree{ module = Module, root = Root},
    Child, Index) ->
    #tree{module = Module, root = Module:add_child(Root, Child, Index)}.


%% children/1
%% --------------------------------------------------------------------
%% @doc return children of node.
%% @end
-spec children(Tree :: term()) -> [term()].
%% --------------------------------------------------------------------
children(#tree{module = Module, root = Root}) ->
    lists:map(fun(Child) -> #tree{module = Module, root = Child} end,
               Module:child_list(Root)).


%% get_child/2
%% --------------------------------------------------------------------
%% @doc return child of node at specified position.
%% @param Index number of child to retrieve or list of indicies describing path to node to return.
%% @end
-spec get_child(Tree :: term(), Index :: non_neg_integer() | [non_neg_integer()]) -> Child :: term() | undefined.
%% --------------------------------------------------------------------
get_child(#tree{module = Module, root = Root}, Index) ->
    case Module:get_child(Root, Index) of
        undefined -> undefined;
        Child -> #tree{module = Module, root = Child}
    end.


%% delete_child/2
%% --------------------------------------------------------------------
%% @doc remove child at position n.
%% @end
-spec delete_child(Tree :: term(), Index :: non_neg_integer()) -> NewTree :: term().
%% --------------------------------------------------------------------
delete_child(Tree = #tree{module = Module, root = Root}, Index) ->
    Tree#tree{root = Module:delete_child(Root, Index)}.


%% set_data/2
%% --------------------------------------------------------------------
%% @doc set user data in the root node.
%% @return will return the changed tree.
%% @end
-spec set_data(Tree :: term(), Data :: any()) -> term().
%% --------------------------------------------------------------------
set_data(#tree{module = Module, root = Root}, Data) ->
    #tree{module = Module, root = Module:set_data(Root, Data)}.


%% get_data/1
%% --------------------------------------------------------------------
%% @doc get user data from root node.
%% @end
-spec get_data(Tree :: term()) -> term().
%% --------------------------------------------------------------------
get_data(#tree{module = Module, root = Root}) ->
    Module:get_data(Root).


%% node_count/1
%% --------------------------------------------------------------------
%% @doc get the number of nodes in the tree.
%% @end
-spec node_count(Tree :: term()) -> pos_integer().
%% --------------------------------------------------------------------
node_count(#tree{module = Module, root = Root}) ->
    Module:node_count(Root).

%% ====================================================================
%% Internal functions
%% ====================================================================


