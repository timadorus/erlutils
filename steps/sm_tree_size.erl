
-module(sm_tree_size).

-export([setup/0, teardown/1, given/3, 'when'/3, then/3]).

-define(TREE_IMPLEMENTATION_MODULE,teu_simple_tree_impl).

setup() ->
    [].

teardown(_State) ->
    ok.

%% Step definitions for the sample calculator Addition feature.
-spec given([atom()], State :: term(), Info :: term()) -> 
    {ok, NewState :: term()} | {error, Reason :: term()}.

given([a, new, tree], _State, _) ->
    {ok, teu_tree:new(?TREE_IMPLEMENTATION_MODULE, [])}.

%% given([], State, _) ->
%%     {ok, State}.


-spec 'when'([atom()], State :: term(), Info :: term()) -> 
    {ok, NewState :: term()} | {error, Reason :: term()}.

'when'([asked, for, the, size, 'of', tree], State, _) ->
    {ok, State}.
    

%% 'when'([], State, _) ->
%%     {ok, State}.


-spec then([atom()], State :: term(), Info :: term()) -> 
    {ok, NewState :: term()} | {error, Reason :: term()}.
    
then([a, size, 'of', CStr, must, be, returned], State, _) ->
    Count = list_to_integer(atom_to_list(CStr)),
    Count =:= teu_tree:size(State).

%% then([], State, _) ->
%%     State.

%% ====================================================================
%% Internal functions
%% ====================================================================
    
    