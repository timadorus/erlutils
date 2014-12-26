%% @author Lutz Behnke <lutz.behnke@gmx.de>
%% @doc a collection of functions to help handling multiple processes.


-module(teu_procs).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([wait_for_exit/1, wait_for_exit/2]).

%% wait_for_exit/1
%% --------------------------------------------------------------------
%% @doc wait for the process with the given pid to terminate.
%% @end
-spec wait_for_exit(Pid :: pid()) -> ok.
%% --------------------------------------------------------------------
wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive 
        {'DOWN', MRef, _, _, _} ->
            ok 
    end.

%% wait_for_exit/2
%% --------------------------------------------------------------------
%% @doc wait for the process with the given pid to terminate.
%% 
%% function will ensure that the process terminated with the 
%% return value given for Reason. If the process has terminated before this 
%% function was called, or has never existed, this function will return 
%% immediately.
%% @end
-spec wait_for_exit(Pid :: pid(), Reason :: term()) -> ok.
%% --------------------------------------------------------------------
 wait_for_exit(Pid, Reason) ->
    MRef = erlang:monitor(process, Pid),
    receive 
        {'DOWN', MRef, _, _, noproc} ->
            ok;
        {'DOWN', MRef, _, _, Info} ->
            ?assertMatch(Reason,Info),
            ok 
    end.



%% ====================================================================
%% Internal functions
%% ====================================================================


