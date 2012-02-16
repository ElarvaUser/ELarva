-module(utilities).

-export([initial_call/1]).

%%-----Taken from the C module------------------------
initial_call(Pid) ->
    case pinfo(Pid) of
	undefined -> undefined;
	Info ->
	    case fetch(initial_call, Info) of
		{proc_lib, init_p, _} ->
	    	    proc_lib:translate_initial_call(Info);
		ICall ->
	    	    ICall
    	    end
    end.

pinfo(Pid) ->
    case is_alive() of
	true -> rpc:call(node(Pid), erlang, process_info, [Pid]);
	false -> process_info(Pid)
    end.

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} -> Val;
	false -> 0
    end.
%%-----Taken from the C module------------------------
