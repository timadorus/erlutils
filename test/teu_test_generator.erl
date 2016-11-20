%% @author sage
%% @doc minimal test generator to be used in unit testing teu_generator

-include_lib("eunit/include/eunit.hrl").

-module(teu_test_generator).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, make_work/1, deliver_work/3, generate/3]).

-record(ctrl_state, {elements = [1,2,3,4,5],
                     handed_out = []}).

%% init/1
%% --------------------------------------------------------------------
%% @doc initialize a controler.
%% @end
-spec init(Args :: [term()]) -> 
          {ok, CtrlState :: term(), GenStartArgs :: list()} 
        | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init(_Args) -> {ok, #ctrl_state{}, []}.

%% make_work/1
%% --------------------------------------------------------------------
%% @doc create a new work package to be sent to one of the generators.
-spec make_work(State :: term()) -> 
        {ok, WorkRef :: term(), WorkSpec :: term(), State :: term()}
      | {error, Reason :: term()}.
%% --------------------------------------------------------------------
make_work(State) -> {ok, make_ref(), {hoo, hah}, State}.

%% deliver_work/3
%% @doc hand in resultant work by the generator to the controler.
%% @end
-spec deliver_work(GenPid :: pid(), Result :: term(), State :: term()) -> 
          {ok, State :: term()}
        | {terminate_gen, State :: term()}
        | {error, Reason ::term()}.
%% --------------------------------------------------------------------
deliver_work(_GenPid, _Result, State) -> {ok, State}.


%% generate/3
%% --------------------------------------------------------------------
%% @doc generate a number of new Elements.
%% @param WorkRef an identifier for the work package. As the process of the 
%%                generator may change to to failures, this is a separate 
%%                reference.
%% @param WorkSpec the work to do.
%% @param CtrlPid of the control process. Send the result of the work here.
%% @end
-spec generate(WorkRef :: reference, WorkSpec :: term, CtrlPid :: pid()) ->
        ok | {error, Reason :: term()}.
%% --------------------------------------------------------------------
generate(_WorkRef, _WorkSpec, _CtrlPid) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


