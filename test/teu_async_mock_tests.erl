%%
%% @doc TODO: Add description to teu_nodes_tests
%%
%% @author sage
%%
%% @copyright 2009-2012 Timadorus Project

-module(teu_async_mock_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([verbose_mock_runner/1]).

-record(fun_call, {module = undefined, function = undefined, args = []}).

%%
%% Fixtures
%%

%% info_test_() ->
%%     { setup, fun() -> ok end,
%%       fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

initial_test_() ->
    { "some initial tests",
      setup,

      fun() ->
              ok
      end,
      fun(_Args) ->
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_send_message()),
                    ?_test(test_stack()),
                    ?_test(test_register()),
                    ?_test(test_verbose()),
					?_test(test_errenous_call_msg())
                   ]
      end }.

verbose_output_test_() ->
	{"setting the verbose option shall write an incoming message to standard out",
	 setup,
	 fun() ->
			 ok
	 end,
	 fun(_Args) -> 
			 ok 
	 end,
	 fun(_Args) ->
			 [
			  ?_test(test_verbose_output())
			 ]
	 end
	}.

wait_for_message_test_() ->
    { "the calling process shall block until it the mock recieves the right message", 
	  setup, 
	  fun() -> 
			  {ok, MPid} = teu_async_mock:start([verbose]),
			  MPid
	  end,
      fun(MPid) ->
			  teu_async_mock:stop(MPid)
	  end,
	  fun(MPid) -> [
					?_test(test_wait_for_any_msg(MPid)),
					?_test(test_wait_for_msg_match(MPid))
                    ]
	  end }.


%%
%% Local Functions
%%

test_verbose_output() ->
	?assertCmdOutput("have seen message: message\n",
					  get_run_functions_command([#fun_call{module = teu_async_mock_tests, 
														   function = verbose_mock_runner, 
														   args = [message]}])),
	ok.

verbose_mock_runner(Msg) ->
	{ok, Pid} = teu_async_mock:start([verbose]),
	gen_server:cast(Pid, Msg),
	teu_async_mock:stop(Pid),
	
	ok.

get_run_functions_command(FunCalls) ->
	get_run_text_command(create_function_call_sequence_string(FunCalls)).

get_run_text_command(Text) ->
	ErlPrefix = "erl -pa _build/test/lib/timadorus_erlutils/ebin -run -eval \"",
	ErlSuffix = "\" -s init stop -noshell",
	Command = ErlPrefix ++ escape_quotes(Text) ++ ErlSuffix,
%% 	?debugFmt("~p~n", [Command]),
	Command.

create_function_call_sequence_string(FunCalls) ->
	string:join(lists:map(fun create_function_call_string/1, FunCalls), ", ") ++ ".".

create_function_call_string(#fun_call{module = Module, function = Function, args = Arguments}) ->
	QualifiedFunctionName = atom_to_list(Module) ++ ":" ++ atom_to_list(Function),
	ArgumentsString = get_arguments_string(Arguments),
	QualifiedFunctionName ++ "(" ++ ArgumentsString ++ ")".

get_arguments_string(Arguments) ->
	ArgumentTexts = [lists:flatten(io_lib:format("~p", [Arg])) || Arg <- Arguments],
	string:join(ArgumentTexts, ", ").

escape_quotes(String) ->
	lists:flatten(lists:map(fun
							   ($\") -> "\\\"";
							   (X) -> X
							end, String)).

test_wait_for_msg_match(MPid) ->

	%% create another process to actually make the call, so we can start the
    %% actual function to wait for the message.
	_TmpPid = spawn(fun() -> 
							timer:sleep(500), 
							gen_server:cast(MPid, {an_invalid_message, notme}), 
							gen_server:cast(MPid, {a_valid_message, 3}) 
					end),
	
%% 	?debugFmt("mock PId: ~p~n",[MPid]),
	
	?assertMatch({ok, {a_valid_message, _V}}, 
				 teu_async_mock:wait_for_msg(MPid, 
                                             fun(Arg) -> 
                                                case Arg of 
                                                   {a_valid_message, _} -> true;
                                                   _ -> false
                                                end 
                                             end)),
	
	ok.

test_wait_for_any_msg(MPid) ->

	%% create another process to actually make the call, so we can start the
    %% actual function to wait for the message.
	_TmpPid = spawn(fun() -> 
							timer:sleep(500), 
							gen_server:cast(MPid, a_valid_message) 
					end),
	
%% 	?debugFmt("mock PId: ~p~n",[MPid]),
	
	?assertMatch({ok, _Msg}, teu_async_mock:wait_for_msg(MPid)),
	
	ok.

test_send_message() ->
	Res = teu_async_mock:start_link([]),
	?assertMatch({ok, _Pid}, Res),
	{ok, Pid} = Res,
	
	?assertMatch(undefined, whereis(true)),
	?assertMatch(undefined, whereis(teu_async_mock)),
	
	gen_server:cast(Pid, test_message),
	
	?assertEqual(test_message, teu_async_mock:last_message(Pid)),
	
    ok.

test_stack() ->
    {ok, Pid} = teu_async_mock:start_link([]),
    
    gen_server:cast(Pid, {message, 1}),
    gen_server:cast(Pid, another_message),
    
    ?assertEqual([another_message, {message, 1}], teu_async_mock:message_stack(Pid)),
    ok.

test_register() ->
	
    {ok, Pid1} = teu_async_mock:start_link([register]),

    ?assertEqual(Pid1, whereis(teu_async_mock)),

    teu_async_mock:stop(Pid1),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(teu_async_mock)),

    {ok, Pid2} = teu_async_mock:start_link([{register, my_proc_name}]),

    ?assertEqual(Pid2, whereis(my_proc_name)),

    teu_async_mock:stop(Pid2),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(my_proc_name)),

    ok.

-dialyzer({no_match, test_verbose/0}).

test_verbose() ->
	
    {ok, Pid} = teu_async_mock:start_link([verbose]),

    gen_server:call(Pid, this_is_a_message_i_am_very_unlikely_to_ever_use),

	gen_server:cast(Pid, test_message),
	
	?assertEqual(test_message, teu_async_mock:last_message(Pid)),
	
	?assert(teu_async_mock:is_verbose(Pid)),

	teu_async_mock:stop(Pid),

    ok.

test_errenous_call_msg() ->
    {ok, Pid} = teu_async_mock:start_link([]),
	
	 gen_server:call(Pid, this_is_a_message_i_am_very_unlikely_to_ever_use),
	
	teu_async_mock:stop(Pid),
	ok.
