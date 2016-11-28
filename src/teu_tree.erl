%% @author sage
%% @doc define an behavoiour for a tree.


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
%% --------------------------------------------------------------------
-callback add_child(Parent :: term(), Child :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------

%% delete_child/3
%% --------------------------------------------------------------------
-callback delete_child(Parent :: term(), Index :: non_neg_integer()) -> Tree :: term().
%% --------------------------------------------------------------------

%% get_child/2
%% @od this may allow a more optimized implementation, otherwise this
%% would be:
%% <pre>
%%  get_child(P,I) -> lists:nth(I,tree_impl:get_list(P).
%% </pre>
%% --------------------------------------------------------------------
-callback get_child(Parent :: term(), Index :: non_neg_integer()) -> Child :: term().

%% --------------------------------------------------------------------

%% child_list/2
%% --------------------------------------------------------------------
-callback add_child(Parent :: term()) -> Children :: [term()].
%% --------------------------------------------------------------------


%% get_data/1
%% --------------------------------------------------------------------
-callback get_data(Tree :: term()) -> Data ::term().
%% --------------------------------------------------------------------

%% set_data/2
%% --------------------------------------------------------------------
-callback set_data(Tree :: term(), Data :: term()) -> Tree ::term().
%% --------------------------------------------------------------------

%% set_data

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/2]).


%% new/2
%% --------------------------------------------------------------------
%% @doc create a new tree.
%% @param Module the module that will implement the tree
%% @param Args any additional args to pass to the module for creating a new tree
%% @end
-spec new(Module :: atom, Args :: list()) -> Tree :: term().
%% --------------------------------------------------------------------
new(Module, Args) -> Module:new(Args).


%% ====================================================================
%% Internal functions
%% ====================================================================


