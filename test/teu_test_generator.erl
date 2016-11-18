%% @author sage
%% @doc minimal test generator to be used in unit testing teu_generator

-include_lib("eunit/include/eunit.hrl").

-module(teu_test_generator).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, init_generator/2, generate/3]).

-record(ctrl_state, {}).
-record(gen_state, {ctrl_pid :: pid()}).

%% init/1
%% --------------------------------------------------------------------
%% @doc initialize a controler.
%% @end
-spec init(Args :: [term()]) -> 
          {ok, GenState :: term()} 
        | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init(_Args) -> #ctrl_state{}.

%% init_generator/2
%% --------------------------------------------------------------------
%% @doc initialize a generator.
%% @param ControlPid the Pid of the controler for this generator
%% @end
-spec init_generator(Args :: [term()], ControlPid :: pid()) -> 
          {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
init_generator(_Args, _ControlPid) -> {ok, #gen_state{}}.

%% generate/3
%% --------------------------------------------------------------------
%% @doc generate a number of new Elements.
%% @param Resource should gives an indication of how many new elements are to be created. 
%%        This may be the actual number of elements or a total cost of all new elements.
%% @param State is the internal state of the generator.
%% @param Pid of the control process. This is provided, so it does not have to be stored
%%        in the state of each generator.
%% @returns a list of new elements. 
%% @end
-spec generate(Chunks :: term, CtrlPid :: pid(), State :: term()) ->
        {ok, State :: term()} | {error, Reason :: term()}.
%% --------------------------------------------------------------------
generate(_Chunk, _CtrlPid, State) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================


