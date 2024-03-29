-module(compiler).

-define(LEXER,lexer).
-define(PARSER,parser).
-define(LOG(Name, Value), 
        io:format("{~p:~p}: ~p -> ~p~n", [?MODULE, ?LINE, Name, Value])).

-export([compile/2]).

compile(System, FileName) ->
    {ok, File} = file:read_file(FileName),
    S = binary_to_list(File),
    ?LOG("File", S),
    {ok, Tokens, EndLine} = ?LEXER:string(S),
    ?LOG("Tokens", Tokens),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ?LOG("ParseTree", ParseTree),
    write_monitor(System, ParseTree),
    write_supervisor(System, ParseTree),
    write_tracer(System, ParseTree),
    write_FSMs(System, ParseTree).

write_monitor(System, ParseTree) ->
    {ok, File} = file:open("monitor.erl", [write]),
    io:format(File, "~s", ["-module(monitor).\n\n" ++
	       "-export([start/0]).\n\n" ++
	       "start() ->\n"]),
    get_properties(ParseTree),

    lists:foreach(
	fun(Property) ->
	    if 
		(Property /= []) ->
	            io:format(File, "~s", [
                        "\t" ++ System ++ "_" ++ erlang:atom_to_list(erlang:element(1, Property)) ++ "_fsm:start_link(),\n"]);
	    	true ->
		    false
	    end
	end, get_properties(ParseTree)),
    io:format(File, "~s", ["\t" ++ System ++ "_supervisor:start_link(),\n" ++
    		"\t{ok, \"Monitor started\"}.\n\n"]),
    file:close(File),
    compile:file("monitor").

write_tracer(System, ParseTree) ->
    {ok, File} = file:open(System ++ "_tracer.erl", [write]),
    io:format(File, "~s", ["-module(" ++ System ++ "_tracer).\n\n" ++
	       "-export([start_link/0, init_trace/0, trace/0]).\n\n" ++
	       "-import(utilities, [initial_call/1]).\n\n" ++
	       "start_link() ->\n" ++
    		   "\t{ok, init_trace()}.\n\n" ++
	       "init_trace() ->\n" ++
    		   "\tspawn_link(?MODULE, trace, []).\n\n" ++
	       "trace() ->\n"]),
%%--------------------------------------------------------------------------
% Check whether any FOREACH exists
%%--------------------------------------------------------------------------
    %case (erlang:element(1, erlang:element(1, erlang:element(2, ParseTree)))) == foreach of
	%true -> io:format(File, "\terlang:trace(whereis(" ++ System ++ "), true, [set_on_spawn, procs, timestamp]),\n", []);
	%false -> false
    %end,
%%--------------------------------------------------------------------------
    DistinctEventsTypes = get_distinct_events_types(ParseTree),
    %% trace() function
    io:format(File, "\terlang:trace(whereis(" ++ System ++ "), true, [set_on_spawn, procs, ", []),
    lists:foreach(fun(EventType) ->
	io:format(File, "~w, ", [EventType])
    end, get_formatted_distinct_events_types([], DistinctEventsTypes)),
    io:format(File, "timestamp]),\n", []),

    %% trace_pattern() function
    lists:foreach(fun(EventType) -> 
	case erlang:element(1, EventType) of
	   call ->
		lists:foreach(fun(EventTuple) ->
		    io:format(File, "\terlang:trace_pattern(~s, true, [local]),\n", [EventTuple])
		end, erlang:element(2, EventType));
	   _Other ->
		false
	end
    end, DistinctEventsTypes),

%% --------------------------------------------------------------------------
%  Inserting specific events related with their respective properties
%% --------------------------------------------------------------------------
    io:format(File, "~s", ["\ttrace_loop().\n\n" ++
    "trace_loop() ->\n" ++
	"\treceive Trace ->\n" ++
	    "\t\t%io:format(\"\~w\~n\", [Trace]),\n" ++
		"\t\tcase Trace of\n"]),

    DistinctEvents = get_distinct_events(ParseTree),
    %Partition events in LINK, SPAWN, OTHERS
    {Link, UnLink, Other} = seperate_distinct_events([], [], [], DistinctEvents),
    % Link events
    write_link_events(File, System, Link, "link"),
    % Unlink events
    write_link_events(File, System, UnLink, "unlink"),

    % Other events
    lists:foreach(fun(DistinctEvent) ->
	EventPattern = erlang:element(2, erlang:element(1, DistinctEvent)),
	case EventType = erlang:element(1, erlang:element(1, DistinctEvent)) of
	call ->
	    {ok, Tokens, _} = erl_scan:string(erlang:element(2, erlang:element(1, DistinctEvent)) ++ "."),
	    {ok, Term} = erl_parse:parse_term(Tokens),
	    io:format(File, "\t\t{trace_ts, Pid, call, {~w, ~w, A}, _} ->\n", [erlang:element(1, Term), erlang:element(2, Term)]);
	spawn ->
	    io:format(File, "\t\t{trace_ts, Pid, spawn, _, ~s, _} ->\n", [erlang:element(2, erlang:element(1, DistinctEvent))]);
	send ->
	    io:format(File, "\t\t{trace_ts, Pid, ~w, ~s, To, _} ->\n", [EventType, EventPattern]);
	channel ->
	    false;
	_ ->
	    io:format(File, "\t\t{trace_ts, Pid, ~w, ~s, _} ->\n", [EventType, EventPattern])
	end,
	Events = erlang:element(2, DistinctEvent),
	{Foreach, WildCard, NonWildCard} = seperate_events([], [], [], Events),
	case erlang:length(NonWildCard) of
	0 ->
	    lists:foreach(fun(Event) ->
	    	case EventType of
	    	    call ->
	    	        io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    'receive' ->
			io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
		    send ->
			io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
		    channel ->
	    		false;
	    	    _ ->
	    	        io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
	    	end
	    end, WildCard);

	_ ->
	    io:format(File, "\t\t\tcase initial_call(Pid) of\n", []),
	    lists:foreach(fun(Event) ->
		io:format(File, "\t\t\t~s ->\n", [erlang:element(3, Event)]),
	    	case EventType of
	    	    call ->
	    	        io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A});\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    'receive' ->
			io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
		    send ->
			io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
		    channel ->
	    		false;
	    	    _ ->
	    	        io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w);\n", [erlang:element(4, Event), erlang:element(2, Event)])
	    	end
	    end, NonWildCard),
	    io:format(File, "\t\t\t_ ->\n", []),
	    case erlang:length(WildCard) of
	    0 ->
		io:format(File, "\t\t\t\tfalse\n", []);
	    _ ->
		lists:foreach(fun(Event) ->
		    case (lists:last(NonWildCard) == Event) of
		    true ->
			case EventType of
	    	            call ->
	    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    'receive' ->
				io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
			    send ->
				io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
			    channel ->
	    			false;
	    	            _ ->
	    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
	    	        end;			     	    
		    false ->
			case EventType of
	    	            call ->
	    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    'receive' ->
				io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
			    send ->
				io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, ~s}),\n", [erlang:element(4, Event), erlang:element(2, Event), EventPattern]);
			    channel ->
	    			false;
	    	            _ ->
	    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
	    	        end
		    end
	        end, WildCard)
	    end,
	    io:format(File, "\t\t\tend,\n", [])
	end,
	
	lists:foreach(fun(Event) ->
	    case EventType of
	    	call ->
	    	    io:format(File, "\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {Pid, ~s}))) ! {'$gen_event', {~w, A}},\n", [erlang:element(3, Event), erlang:element(2, Event)]);
		'receive' ->
		    io:format(File, "\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {Pid, ~s}))) ! {'$gen_event', {~w, ~s}},\n", [erlang:element(3, Event), erlang:element(2, Event), EventPattern]);
		send ->
		    io:format(File, "\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {Pid, ~s}))) ! {'$gen_event', {~w, ~s}},\n", [erlang:element(3, Event), erlang:element(2, Event), EventPattern]);
		channel ->
	    	    false;
	    	_ ->
	    	    io:format(File, "\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {Pid, ~s}))) ! {'$gen_event', ~w},\n", [erlang:element(3, Event), erlang:element(2, Event)])
	    end
	end, Foreach),

	case EventType of
	channel ->
	    false;
	_ ->
	    io:format(File, "\t\t\ttrace_loop();\n\n", [])
	end
    end, Other),

%% --------------------------------------------------------------------------
%  FOREACH
%% --------------------------------------------------------------------------
%%--------------------------------------------------------------------------
% Check whether any FOREACH exists
%%--------------------------------------------------------------------------
    case (erlang:element(1, erlang:element(1, erlang:element(2, ParseTree)))) == foreach of
	true -> io:format(File, "\t\t{trace_ts, Pid, exit, _, _} ->\n" ++
	    "\t\t\tlists:foreach(fun(FsmPid) ->\n" ++
	    "\t\t\t\tgen_fsm:send_event(lists:last(FsmPid), stop)\n" ++
	    "\t\t\tend, ets:match(hashTable, {{Pid, '_'}, '$1'})),\n" ++
	    "\t\t\tets:match_delete(hashTable, {{Pid, '_'}, '_'}),\n" ++
	    "\t\t\ttrace_loop();\n\n", []);
	false -> false
    end,
%%--------------------------------------------------------------------------
    lists:foreach(fun(Foreach) ->
	io:format(File, "\t\t{trace_ts, _, spawn, Pid, ~s, _} ->\n", [erlang:element(1, Foreach)]),
	lists:foreach(fun(Property) ->
	    io:format(File, "\t\t\tets:insert(hashTable, {{Pid, " ++ erlang:atom_to_list(erlang:element(1, Property)) ++ "}, erlang:element(2, " ++ System ++ "_" ++ erlang:atom_to_list(erlang:element(1, Property)) ++ "_fsm:start_link(Pid))}),\n", []),
	    % Enter trace_pattern() function call
            lists:foreach(fun(Event) -> 
	        case erlang:element(1, erlang:element(2, Event)) of
	           call ->
	                io:format(File, "~s", ["\t\t\tio:format(\"Tracer - Traced Processes: ~w~n\", [erlang:trace_pattern(" ++ erlang:element(2, erlang:element(2, Event))  ++ ", true, [local])]),\n"]);
	           _Other ->
		        false
	        end
            end, get_events(Property))
        end, erlang:element(2, Foreach)),
	io:format(File, "\t\t\ttrace_loop();\n\n", [])
    end, get_foreach(ParseTree)),
%% --------------------------------------------------------------------------
    
    io:format(File, "~s", ["\t\t_ ->\n" ++
			"\t\t\ttrace_loop()\n" ++
		"\t\tend\n" ++
    		"\tend.\n\n"]),
%% --------------------------------------------------------------------------
    file:close(File),
    compile:file(System ++ "_tracer").

write_supervisor(System, ParseTree) ->
    {ok, File} = file:open(System ++ "_supervisor.erl", [write]),
    io:format(File, "~s", ["-module(" ++ System ++ "_supervisor).\n\n" ++
	       "-behaviour(supervisor).\n\n" ++
	       "-export([start_link/0, init/1]).\n\n" ++
	       "start_link() ->\n" ++
    		   "\tsupervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n" ++
	       "init([]) ->\n" ++
   		   "\tets:new(hashTable, [named_table, bag, public]),\n" ++
	           "\t{ok, {{one_for_one, 1,60}, [{" ++ System ++ ", {" ++ System ++ ", start_link, []}, permanent, brutal_kill, worker, [" ++ System ++ "]},\n"++
			                         "\t{" ++ System ++ "_tracer, {" ++ System ++ "_tracer, start_link, []}, permanent, brutal_kill, worker, [" ++ System ++ "_tracer]}]}}.\n\n"]),
    file:close(File),
    compile:file(System ++ "_supervisor").

write_FSMs(System, ParseTree) ->
    lists:foreach(
	fun(Property) ->
	    Events = get_events(Property),
	    States = get_all_states(Property),
	    Filename = System ++ "_" ++ erlang:atom_to_list(erlang:element(1, Property)) ++ "_fsm",
	    {ok, File} = file:open(Filename ++ ".erl", [write]),
	    io:format(File, "~s", ["-module(" ++ Filename ++ ").\n\n" ++
	       "-behaviour(gen_fsm).\n\n" ++
	       "-export([" ++ 
               lists:map(fun(State) -> erlang:atom_to_list(State) ++ "/2, " end, States) ++
	       "start_link/0, start_link/1, init/1, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).\n\n" ++
	       "-define(SERVER, ?MODULE).\n\n" ++
	       "start_link() ->\n" ++
  	       "\tgen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).\n\n" ++
	       "start_link(Args) ->\n" ++
	       "\tgen_fsm:start_link(?MODULE, Args, []).\n\n" ++
	       "init(State) ->\n" ++
  	       "\t{ok, " ++ erlang:atom_to_list(lists:last(States)) ++ ", State}.\n\n"]),

	    BadStates = get_bad_states(Property),
	    %% Foreach State
	    lists:foreach(
		fun(State) ->
		    io:format(File, "~s", [erlang:atom_to_list(State) ++ "(Event, State) ->\n" ++
			"\tio:format(\""++ Filename ++" - ~w - received event - ~w - while in state - "++ erlang:atom_to_list(State) ++"~n\", [self(), Event]),\n"]),
		    case lists:member(State, BadStates) of
			%% Bad states
		    	true  -> io:format(File, "~s", ["\tio:format(\"FSM reached bad state!!!~n\"),\n" ++
					"\t{next_state, " ++ erlang:atom_to_list(State) ++ ", State}.\n\n"]);
			%% Normal and starting states
			false -> Transitions = get_filtered_transitions(State, Property),
				 case erlang:length(Transitions) of
				     %% _ transistions
				     0 ->  io:format(File, "~s", ["\t{next_state, " ++ erlang:atom_to_list(State) ++ ", State}.\n\n"]);
				     _ ->  io:format(File, "~s", ["\tcase Event of\n"]),
					   %% Foreach transition
					   lists:foreach(
						%% {EventTuple, [Transitions]}
				                fun(Transition) ->
						     EventName = erlang:element(2, erlang:element(1, Transition)),
						     EventType = erlang:element(1, erlang:element(2, lists:last(lists:filter(fun(Event) -> EventName == erlang:element(1, Event) end, Events)))),
						     EventTuple = erlang:element(2, erlang:element(2, lists:last(lists:filter(fun(Event) -> EventName == erlang:element(1, Event) end, Events)))), 
						     case EventType of
						     call ->
		    			             	io:format(File, "~s~w~s", ["\t\t{", EventName, ", A} ->\n"]);
						     send ->
							io:format(File, "~s~w~s", ["\t\t{", EventName, ", " ++ EventTuple ++ "} ->\n"]);
						     'receive' ->
							io:format(File, "~s~w~s", ["\t\t{", EventName, ", " ++ EventTuple ++ "} ->\n"]);
						     channel ->
							io:format(File, "~s~w~s", ["\t\t{", EventName, ", " ++ EventTuple ++ "} ->\n"]);
						     _ ->
							io:format(File, "~s~w~s", ["\t\t", EventName," ->\n"])
						     end,

						     CPA = check_condition_plus_action(false, erlang:element(2, Transition)),
						     case CPA of
							true ->
						     	%% Condition plus Action
						     	condition_part_1(File, 6, 1, erlang:element(2, Transition)),
						     	io:format(File, "\t\t\tif\n", []),
						     	condition_plus_action_part_2(File, 1, erlang:element(2, Transition), System, Filename),
							io:format(File, "~s", ["\t\t\t\ttrue ->\n\t\t\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(State) ++ "\", [self()]),\n" ++ "\t\t\t\t\t{next_state, " ++ erlang:atom_to_list(State) ++ ", State}\n\t\t\tend"]);

							_ ->
							false
						     end,

						     CC = check_condition(false, erlang:element(2, Transition)),

						     case CC of
							true ->
						     	%% Condition
						     	condition_part_1(File, 5, 1, erlang:element(2, Transition)),
						     	io:format(File, "\t\t\tif\n", []),
						     	condition_part_2(File, 1, erlang:element(2, Transition), Filename),
						     	io:format(File, "~s", ["\t\t\t\ttrue ->\n\t\t\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(State) ++ "\", [self()]),\n" ++ "\t\t\t\t\t{next_state, " ++ erlang:atom_to_list(State) ++ ", State}\n\t\t\tend"]);

							_ ->
							false
						     end,

						     CA = check_action(false, erlang:element(2, Transition)),

						     case CA of
							true ->
						     	%% Action
						     	action(File, erlang:element(2, Transition), System, Filename);

							_ ->
							false
						     end,

						     %% Transition without Condition and Action
						     transition_with_no_arguments(File, erlang:element(2, Transition), System, Filename, State),

						     case (lists:last(Transitions) == Transition) of
						        true ->
					     	            io:format(File, ";\n\t\t_ ->\n\t\t\t{next_state, ~s, State}\n", [State]);
						        false ->
						           io:format(File, "~s", [";\n"])
					             end
				                end, Transitions),
					   io:format(File, "~s", ["\tend.\n\n"])
				 end
		    end
		end, States),
	    io:format(File, "~s", [
	       "state_name(_Event, _From, State) ->\n" ++
  	       "\tReply = ok,\n" ++
  	       "\t{reply, Reply, start, State}.\n\n" ++
	       "handle_event(stop, StateName, State) ->\n" ++
	       "\t{stop, normal, State};\n" ++
	       "handle_event(_Event, StateName, State) ->\n" ++
  	       "\t{next_state, StateName, State}.\n\n" ++
	       "handle_sync_event(Event, From, StateName, State) ->\n" ++
  	       "\tReply = ok,\n" ++
  	       "\t{reply, Reply, StateName, State}.\n\n" ++
	       "handle_info(_Info, StateName, State) ->\n" ++
  	       "\t{next_state, StateName, State}.\n\n" ++
	       "terminate(_Reason, _StateName, _State) ->\n" ++
  	       "\tok.\n\n" ++
	       "code_change(_OldVsn, StateName, State, _Extra) ->\n" ++
  	       "\t{ok, StateName, State}."]),
 	    file:close(File),
	    compile:file(Filename)
	end, get_all_properties(ParseTree)).

write_link_events(File, System, List, Type) ->
    % ----------------------------------LINK events - Begin-----------------------------------------------------
    case erlang:length(List) of
    0 ->
	false;
    _ ->
	io:format(File, "\t\t{trace_ts, From, ~s, To, _} ->\n", [Type]),
	{LinkWildCard, LinkNonWildCard} = seperate_link_distinct_events([], [], List),
	case erlang:length(LinkNonWildCard) of
	0 ->
	    lists:foreach(fun(DistinctEvent) ->
		EventType = erlang:element(1, erlang:element(1, DistinctEvent)),
		Events = erlang:element(2, DistinctEvent),
		{Foreach, WildCard, NonWildCard} = seperate_events([], [], [], Events),
		case erlang:length(NonWildCard) of
		0 ->
		    lists:foreach(fun(Event) ->
		    	case EventType of
		    	    call ->
		    	        io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	    _ ->
		    	        io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	end
		    end, WildCard);

		_ ->
		    io:format(File, "\t\t\tcase initial_call(From) of\n", []),
%% TO DO ------------------------- Get distinct From Tuples
		    lists:foreach(fun(Event) ->
			io:format(File, "\t\t\t~s ->\n", [erlang:element(3, Event)]),
		    	case (lists:last(NonWildCard) == Event) of
			    true ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A});\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w);\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end;			     	    
			    false ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end
			end
		    end, NonWildCard),
		    io:format(File, "\t\t\t_ ->\n", []),
		    case erlang:length(WildCard) of
		    0 ->
			io:format(File, "\t\t\t\tfalse\n", []);
		    _ ->
			lists:foreach(fun(Event) ->
			    case (lists:last(WildCard) == Event) of
			    true ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end;			     	    
			    false ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end
			    end
			end, WildCard)
		    end,
		    io:format(File, "\t\t\tend,\n", [])
		end,

		lists:foreach(fun(Event) ->
		    case EventType of
		    	call ->
		    	    io:format(File, "\t\t\t\tgen_fsm:send_event(ets:lookup(hashTable, {Pid, ~s})), {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	_ ->
		    	    io:format(File, "\t\t\t\tgen_fsm:send_event(ets:lookup(hashTable, {Pid, ~s})), ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    end
		end, Foreach),
		io:format(File, "\t\t\ttrace_loop();\n\n", [])
	end, LinkWildCard);

	_ ->
	    io:format(File, "\t\t\tcase initial_call(To) of\n", []),
	    lists:foreach(fun(DistinctEvent) ->
		EventType = erlang:element(1, erlang:element(1, DistinctEvent)),
		io:format(File, "\t\t\t~s ->\n", [erlang:element(2, erlang:element(1, DistinctEvent))]),
		Events = erlang:element(2, DistinctEvent),
		{Foreach, WildCard, NonWildCard} = seperate_events([], [], [], Events),
		lists:foreach(fun(Event) ->
		    io:format(File, "\t\t\t\tlists:foreach(fun(Fsm) ->\n", []),
		    case EventType of
		    	call ->
		    	    io:format(File, "\t\t\t\t\tgen_fsm:send_event(erlang:element(2, Fsm), {~w, A})\n", [erlang:element(2, Event)]);
		    	_ ->
		    	    io:format(File, "\t\t\t\t\tgen_fsm:send_event(erlang:element(2, Fsm), ~w)\n", [erlang:element(2, Event)])
		    end,
		    io:format(File, "\t\t\t\tend, ets:lookup(hashTable, Pid)),\n", [])
		end, Foreach),
		case erlang:length(NonWildCard) of
		0 ->
		    lists:foreach(fun(Event) ->
		    	case (lists:last(WildCard) == Event) of
			    true ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A});\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w);\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end;			     	    
			    false ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end
			end
		    end, WildCard);

		_ ->
		    io:format(File, "\t\t\t\tcase initial_call(From) of\n", []),
		    lists:foreach(fun(Event) ->
			io:format(File, "\t\t\t\t~s ->\n", [erlang:element(3, Event)]),
		    	case EventType of
		    	    call ->
		    	        io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A});\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	    _ ->
		    	        io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w);\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	end
		    end, NonWildCard),
		    io:format(File, "\t\t\t\t_ ->\n", []),
		    case erlang:length(WildCard) of
		    0 ->
			io:format(File, "\t\t\t\t\tfalse\n", []);
		    _ ->
			lists:foreach(fun(Event) ->
			    case (lists:last(WildCard) == Event) of
			    true ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end;			     	    
			    false ->
				case EventType of
		    	            call ->
		    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
		    	            _ ->
		    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
		    	        end
			    end
			end, WildCard)
		    end,
		    io:format(File, "\t\t\t\tend;\n", [])
		end
	    end, LinkNonWildCard),	

	    io:format(File, "\t\t\t_ ->\n", []),
	    case erlang:length(LinkWildCard) of
	    0 ->
		io:format(File, "\t\t\t\tfalse\n", []);
	    _ ->
		lists:foreach(fun(DistinctEvent) ->
			EventType = erlang:element(1, erlang:element(1, DistinctEvent)),
			Events = erlang:element(2, DistinctEvent),
			{Foreach, WildCard, NonWildCard} = seperate_events([], [], [], Events),
			lists:foreach(fun(Event) ->
			    case EventType of
			    	call ->
			    	    io:format(File, "\t\t\t\tgen_fsm:send_event(ets:lookup(hashTable, {Pid, ~s})), {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	_ ->
			    	    io:format(File, "\t\t\t\tgen_fsm:send_event(ets:lookup(hashTable, {Pid, ~s})), ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    end
			end, Foreach),
			case erlang:length(NonWildCard) of
			0 ->
			    lists:foreach(fun(Event) ->
			    	case (lists:last(WildCard) == Event) of
				    true ->
					case EventType of
			    	            call ->
			    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	            _ ->
			    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    	        end;			     	    
				    false ->
					case EventType of
			    	            call ->
			    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	            _ ->
			    	                io:format(File, "\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    	        end
				end
			    end, WildCard);

			_ ->
			    io:format(File, "\t\t\t\tcase initial_call(From) of\n", []),
			    lists:foreach(fun(Event) ->
				io:format(File, "\t\t\t\t~s ->\n", [erlang:element(3, Event)]),
			    	case EventType of
			    	    call ->
			    	        io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A});\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	    _ ->
			    	        io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w);\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    	end
			    end, NonWildCard),
			    io:format(File, "\t\t\t\t_ ->\n", []),
			    case erlang:length(WildCard) of
			    0 ->
				io:format(File, "\t\t\t\t\tfalse\n", []);
			    _ ->
				lists:foreach(fun(Event) ->
				    case (lists:last(NonWildCard) == Event) of
				    true ->
					case EventType of
			    	            call ->
			    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A})\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	            _ ->
			    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w)\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    	        end;			     	    
				    false ->
					case EventType of
			    	            call ->
			    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, {~w, A}),\n", [erlang:element(4, Event), erlang:element(2, Event)]);
			    	            _ ->
			    	                io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~w),\n", [erlang:element(4, Event), erlang:element(2, Event)])
			    	        end
				    end
				end, WildCard)
			    end,
			    io:format(File, "\t\t\\ttend\n", [])
			end
		end, LinkWildCard)
	    end,
	    io:format(File, "\t\t\tend,\n" ++ "\t\t\ttrace_loop();\n\n", [])
	end
    end.
    % ----------------------------------LINK events - Finish-----------------------------------------------------

find_element(Element, Tuple) ->
    case erlang:element(1, Tuple) of
	Element -> erlang:element(2, Tuple);
	_       -> ?LOG("Element Not Found", erlang:element(1, erlang:element(2, Tuple))),
		   find_element(Element, erlang:element(1, erlang:element(2, Tuple)))
    end.

seperate_distinct_events(Link, UnLink, Other, []) -> {Link, UnLink, Other};
seperate_distinct_events(Link, UnLink, Other, [Head|Tail]) ->
    case erlang:element(1, erlang:element(1, Head)) of
	link ->
	    NewLink = Link ++ [Head],
	    NewUnLink = UnLink,
	    NewOther = Other;
	unlink ->
	    NewLink = Link,
	    NewUnLink = UnLink ++ [Head],
	    NewOther = Other;
	_ ->
	    NewLink = Link,
	    NewUnLink = UnLink,
	    NewOther = Other ++ [Head]
    end,
    seperate_distinct_events(NewLink, NewUnLink, NewOther, Tail).

seperate_link_distinct_events(LinkWildCard, LinkNonWildCard, []) -> {LinkWildCard, LinkNonWildCard};
seperate_link_distinct_events(LinkWildCard, LinkNonWildCard, [Head|Tail]) ->
    case erlang:element(2, erlang:element(1, Head)) of
	"_" ->
    	    NewLinkWildCard = LinkWildCard ++ [Head],
	    NewLinkNonWildCard = LinkNonWildCard;
	Tuple ->
    	    NewLinkNonWildCard = LinkNonWildCard ++ [Head],
	    NewLinkWildCard = LinkWildCard
    end,
    seperate_link_distinct_events(NewLinkWildCard, NewLinkNonWildCard, Tail).

seperate_events(Foreach, WildCard, NonWildCard, []) -> {Foreach, WildCard, NonWildCard};
seperate_events(Foreach, WildCard, NonWildCard, [Head|Tail]) ->
    case erlang:element(1, Head) of
	foreach ->
	    NewForeach = Foreach ++ [Head],
	    NewWildCard = WildCard,
	    NewNonWildCard = NonWildCard;
	property ->
	    NewForeach = Foreach,
	    case erlang:element(3, Head) of
		"_" ->
	    	    NewWildCard = WildCard ++ [Head],
		    NewNonWildCard = NonWildCard;
		Tuple ->
	    	    NewNonWildCard = NonWildCard ++ [Head],
		    NewWildCard = WildCard
	    end
    end,
    seperate_events(NewForeach, NewWildCard, NewNonWildCard, Tail).

%% ---------------------------------------------------------------------
%  Funtions which parse all the events and return a list of events in the
%  pattern: [{{event_type, event_tuple}, [{event_parent, event_name, property_name}, ...]}, ...]
%% ---------------------------------------------------------------------
merge_events(ForeachList, PropertyList) ->
    merge_function(PropertyList, PropertyList, ForeachList).

merge_function(MergedEvents, PropertyList, []) -> MergedEvents;
merge_function(MergedEvents, PropertyList, [Head|Tail]) ->
    Key = erlang:element(1, Head),
    case lists:keyfind(Key, 1, PropertyList) of
	false ->
	    NewMergedEvents = MergedEvents ++ [Head];
	Tuple ->
	    NewMergedEvents = lists:keyreplace(Key, 1, PropertyList, erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ erlang:element(2, Head))))
    end,
    merge_function(NewMergedEvents, PropertyList, Tail).

get_distinct_events(ParseTree) ->
    case erlang:element(1, erlang:element(1, erlang:element(2, ParseTree))) of
	foreach -> 
	    if 
		tuple_size(erlang:element(2, ParseTree)) == 1 ->
		    loop_foreach([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)))); 
		tuple_size(erlang:element(2, ParseTree)) > 1 ->
		    merge_events(loop_foreach([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)))), loop_properties([], erlang:element(2, erlang:element(2, erlang:element(2, ParseTree)))));
		true ->
		    []
	    end;
	properties -> loop_properties([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree))));
	_ -> [], ?LOG("Error - No Properties Specified", [])
    end.

loop_properties(Events,[]) -> Events;
loop_properties(Events,[Head|Tail]) ->
    NewEvents = loop_events(Events, erlang:element(1, Head), erlang:element(2, erlang:element(1, erlang:element(2, Head)))),
    loop_properties(NewEvents, Tail).

loop_events(Events, PropertyName, []) -> Events;
loop_events(Events, PropertyName, [Head|Tail]) ->
    %% Event example = {{'receive', {receive}}, [{property, 'ReceiveMsg', _, users}]}
    %Key = erlang:element(2, Head),
    Key = {erlang:element(1, erlang:element(2, Head)), erlang:element(2, erlang:element(2, Head))},
    case lists:keyfind(Key, 1, Events) of
    false ->
	%NewTuple = {Key, [{property, erlang:element(1, Head), PropertyName}]},
	NewTuple = {Key, [{property, erlang:element(1, Head), erlang:element(3, erlang:element(2, Head)), PropertyName}]},
	NewEvents = Events ++ [NewTuple],
	loop_events(NewEvents, PropertyName, Tail);
    Tuple ->
	%NewTuple = erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ [{property, erlang:element(1, Head), PropertyName}])),
	NewTuple = erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ [{property, erlang:element(1, Head), erlang:element(3, erlang:element(2, Head)), PropertyName}])),
	NewEvents = lists:keyreplace(Key, 1, Events, NewTuple),
	loop_events(NewEvents, PropertyName, Tail)
    end.

loop_foreach(Events,[]) -> Events;
loop_foreach(Events,[Head|Tail]) ->
    NewEvents = loop_foreach_properties(Events, erlang:element(2, Head)),
    loop_foreach(NewEvents, Tail).

loop_foreach_properties(Events, []) -> Events;
loop_foreach_properties(Events, [Head|Tail]) ->
    NewEvents = loop_foreach_events(Events, erlang:element(1, Head), erlang:element(2, erlang:element(1, erlang:element(2, Head)))),
    loop_foreach_properties(NewEvents, Tail).

loop_foreach_events(Events, PropertyName, []) -> Events;
loop_foreach_events(Events, PropertyName, [Head|Tail]) ->
    %% Event example = {{'receive', {receive}}, [{foreach, 'ReceiveMsg', property_name}]}
    Key = erlang:element(2, Head),
    case lists:keyfind(Key, 1, Events) of
    false ->
	NewTuple = {Key, [{foreach, erlang:element(1, Head), PropertyName}]},
	NewEvents = Events ++ [NewTuple],
	loop_foreach_events(NewEvents, PropertyName, Tail);
    Tuple ->
	NewTuple = erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ [{foreach, erlang:element(1, Head), PropertyName}])),
	NewEvents = lists:keyreplace(Key, 1, Events, NewTuple),
	loop_foreach_events(NewEvents, PropertyName, Tail)
    end.

%%-----------------------------------------------------------------------

%% ---------------------------------------------------------------------
%  Funtions which parse all the events and return a list of events types in
%  the pattern: [{event_type, [{M, F, A}, ...]}, ...]
%% ---------------------------------------------------------------------
get_formatted_distinct_events_types(FormattedEvents, []) -> FormattedEvents;
get_formatted_distinct_events_types(FormattedEvents, [Head|Tail]) -> 
	case erlang:element(1, Head) of
	   spawn ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   exit ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   register ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   unregister ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   link ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   unlink ->
		case FormattedEvents of
		[procs] ->
		    NewFormattedEvents = FormattedEvents;
		[] ->
		    NewFormattedEvents = FormattedEvents ++ [procs]
		end;
	   'receive' ->
		NewFormattedEvents = FormattedEvents ++ ['receive'];
	   send ->
		NewFormattedEvents = FormattedEvents ++ [send];
	   call ->
		NewFormattedEvents = FormattedEvents ++ [call];
	   procs ->
		NewFormattedEvents = FormattedEvents ++ [procs];
	   channel ->
		NewFormattedEvents = FormattedEvents
	end,
    get_formatted_distinct_events_types(NewFormattedEvents, Tail).

merge_events_types(ForeachList, PropertyList) ->
    lists:merge(ForeachList, PropertyList).

get_distinct_events_types(ParseTree) ->
    case erlang:element(1, erlang:element(1, erlang:element(2, ParseTree))) of
        foreach -> 
	    if 
		tuple_size(erlang:element(2, ParseTree)) == 1 ->
		    loop_foreach_types([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)))); 
		tuple_size(erlang:element(2, ParseTree)) > 1 ->
		    merge_events_types(loop_foreach_types([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)))), loop_properties_types([], erlang:element(2, erlang:element(2, erlang:element(2, ParseTree)))));
		true ->
		    []
	    end;
	properties -> loop_properties_types([], erlang:element(2, erlang:element(1, erlang:element(2, ParseTree))));
	_ -> ?LOG("Error - No Properties Specified", [])
    end.

loop_properties_types(Events,[]) -> Events;
loop_properties_types(Events,[Head|Tail]) ->
    NewEvents = loop_events_types(Events, erlang:element(2, erlang:element(1, erlang:element(2, Head)))),
    loop_properties_types(NewEvents, Tail).

loop_foreach_types(Events,[]) -> Events;
loop_foreach_types(Events,[Head|Tail]) ->
    NewEvents = loop_foreach_properties_types(Events, erlang:element(2, Head)),
    loop_foreach_types(NewEvents, Tail).

loop_foreach_properties_types(Events, []) -> Events;
loop_foreach_properties_types(Events, [Head|Tail]) ->
    NewEvents = loop_events_types(Events, erlang:element(2, erlang:element(1, erlang:element(2, Head)))),
    loop_foreach_properties_types(NewEvents, Tail).

loop_events_types(Events, []) -> Events;
loop_events_types(Events, [Head|Tail]) ->
    %% Event example = {call, [{M, F, A}]}, {'receive', []}, {send, []}
    Key = erlang:element(1, erlang:element(2, Head)),
    case lists:keyfind(Key, 1, Events) of
    false ->
	case Key of
	call ->
	    NewTuple = {Key, [erlang:element(2, erlang:element(2, Head))]};
	_ ->
	    NewTuple = {Key, []}
	end,
 	NewEvents = Events ++ [NewTuple],
	loop_events_types(NewEvents, Tail);

    Tuple ->
	case Key of
	call ->
	    NewTuple = erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ [erlang:element(2, erlang:element(2, Head))])),
	    NewEvents = lists:keyreplace(Key, 1, Events, NewTuple),
	    loop_events_types(NewEvents, Tail);
	_ ->
	    loop_events_types(Events, Tail)
	end
    end.

%%-----------------------------------------------------------------------

get_events(Property) ->
    erlang:element(2, erlang:element(1, erlang:element(2, Property))).

get_foreach(ParseTree) ->
    case erlang:element(1, erlang:element(1, erlang:element(2, ParseTree))) of
	foreach -> erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)));
	properties -> [];
	_ -> ?LOG("Error", []), []
    end.

get_all_properties(ParseTree) ->
    case erlang:element(1, erlang:element(1, erlang:element(2, ParseTree))) of
	foreach -> 
	    if
		tuple_size(erlang:element(2, ParseTree)) > 1 ->
		    get_foreach_properties([],erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)))) ++ erlang:element(2, erlang:element(2, erlang:element(2, ParseTree)));
		tuple_size(erlang:element(2, ParseTree)) == 1 ->
		    get_foreach_properties([],erlang:element(2, erlang:element(1, erlang:element(2, ParseTree))));
		true ->
		    []
	    end;
	properties -> erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)));
	_ -> ?LOG("Error - No Properties Specified", [])
    end.

get_properties(ParseTree) ->
    case erlang:element(1, erlang:element(1, erlang:element(2, ParseTree))) of
	foreach -> 
	    if 
		tuple_size(erlang:element(2, ParseTree)) > 1 ->
		    erlang:element(2, erlang:element(2, erlang:element(2, ParseTree)));
		true ->
		    []
	    end;
	properties -> erlang:element(2, erlang:element(1, erlang:element(2, ParseTree)));
	_ -> [], ?LOG("Error - No Properties Specified", [])
    end.

get_foreach_properties(Properties,[]) -> Properties;
get_foreach_properties(Properties,[Head|Tail]) ->
    NewProperties = Properties ++ erlang:element(2, Head),
    get_foreach_properties(NewProperties, Tail).

get_filtered_transitions(State, Property) ->
    %lists:filter(fun(Transition) -> State == erlang:element(1, Transition) end, get_transitions(Property)).
    filter_transitions([], lists:filter(fun(Transition) -> State == erlang:element(1, Transition) end, get_transitions(Property))).

filter_transitions(Transitions, []) -> Transitions;
filter_transitions(Transitions, [Head|Tail]) ->
    %% Transition example = {current, to, next, EventTuple, ConditionTuple, ActionTuple}
    Key = erlang:element(4, Head),
    case lists:keyfind(Key, 1, Transitions) of
    false ->
	NewTuple = {Key, [Head]},
 	NewTransitions = Transitions ++ [NewTuple],
	filter_transitions(NewTransitions, Tail);

    Tuple ->
        NewTuple = erlang:setelement(2, Tuple, (erlang:element(2, Tuple) ++ [Head])),
        NewTransitions = lists:keyreplace(Key, 1, Transitions, NewTuple),
        filter_transitions(NewTransitions, Tail)
    end.

get_transitions(Property) ->
    case erlang:element(1, erlang:element(2, erlang:element(2, Property))) of
	states -> erlang:element(2, erlang:element(3, erlang:element(2, Property)));
	_ -> ?LOG("Error", [])
    end.

get_all_states(Property) ->
    case erlang:element(1, erlang:element(2, erlang:element(2, Property))) of
	states -> erlang:element(2, erlang:element(2, erlang:element(2, erlang:element(2, Property)))) ++ 
		  erlang:element(2, erlang:element(3, erlang:element(2, erlang:element(2, Property)))) ++
		  [erlang:element(2, erlang:element(4, erlang:element(2, erlang:element(2, Property))))];
	_ -> ?LOG("Error", [])
    end.

get_bad_states(Property) ->
    case erlang:element(1, erlang:element(2, erlang:element(2, Property))) of
	states -> erlang:element(2, erlang:element(2, erlang:element(2, erlang:element(2, Property))));
	_ -> ?LOG("Error", [])
    end.

%% Condition Part 1
condition_part_1(File, Type, N, []) -> [];
condition_part_1(File, Type, N, [SimilarTransition|Tail]) ->
    case erlang:tuple_size(SimilarTransition) of
    	Type ->
	    case erlang:element(1, erlang:element(5, SimilarTransition)) of
		condition ->
	    	    io:format(File, "\t\t\t_Condition~w = " ++ erlang:element(2, erlang:element(5, SimilarTransition)) ++ ",\n", [N]);
		_ -> true
	    end;

	_ -> true
    end,
    condition_part_1(File, Type, N + 1, Tail).

condition_plus_action_part_2(File, N, [], System, Filename) -> [];
condition_plus_action_part_2(File, N, [SimilarTransition|Tail], System, Filename) ->
    case erlang:tuple_size(SimilarTransition) of
	6 ->
	io:format(File, "\t\t\t\t_Condition~w == true ->\n", [N]),
	%% Action
	{ok, ActionsTokens, _} = erl_scan:string(erlang:element(2, erlang:element(6, SimilarTransition)) ++ "."),
	{ok, Actions} = erl_parse:parse_term(ActionsTokens),
	case erlang:element(1, Actions) of
	channel ->
	    lists:foreach(
	    	fun(Action) ->
		    case erlang:element(1, Action) of
		    global ->
		    	io:format(File, "\t\t\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~s),\n", [erlang:element(2, Action), erlang:element(3, Action)]);
		    foreach ->
		    	io:format(File, "\t\t\t\t\tlists:foreach(fun(FsmPid) ->\n" ++
			"\t\t\t\t\t\tlists:last(FsmPid) ! {'$gen_event', ~s}\n" ++
			"\t\t\t\t\tend, ets:match(hashTable, {{'_', ~w}, '$1'})),\n", [erlang:element(3, Action), erlang:element(2, Action)]);
		    parent ->
		    	io:format(File, "\t\t\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {State, ~w}))) ! {'$gen_event', ~s},\n", [erlang:element(2, Action), erlang:element(3, Action)]);
		    _ ->
		    	false
		    end
		end, erlang:element(2, Actions));
	variable ->
	    lists:foreach(
	    	fun(Action) ->
		    io:format(File, "\t\t\t\t\t~s,\n", [Action])
	    	end, erlang:element(2, Actions))
	end,
	io:format(File, "~s", ["\t\t\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ "~n\", [self()]),\n" ++
	"\t\t\t\t\t{next_state, " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ ", State};\n"]);
	_ ->
	    true
    end,
    condition_plus_action_part_2(File, N + 1, Tail, System, Filename).

condition_part_2(File, N, [], Filename) -> [];
condition_part_2(File, N, [SimilarTransition|Tail], Filename) ->
    case erlang:tuple_size(SimilarTransition) of
	5 ->
	    case erlang:element(1, erlang:element(5, SimilarTransition)) of
		condition ->
		    %% Condition
		    io:format(File, "~s", ["\t\t\t\t_Condition" ++ erlang:integer_to_list(N) ++ " == true ->\n" ++
		"\t\t\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ "\", [self()]),\n" ++ "\t\t\t\t\t{next_state, " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ ", State};\n"]);
		_ ->
		    false
	    end;
	_ ->
	    false
    end,
    condition_part_2(File, N + 1, Tail, Filename).

action(File, [], System, Filename) -> [];
action(File, [SimilarTransition|Tail], System, Filename) ->
    case erlang:tuple_size(SimilarTransition) of
	5 ->
	    case erlang:element(1, erlang:element(5, SimilarTransition)) of
		action ->
		    %% Action
			{ok, ActionsTokens, _} = erl_scan:string(erlang:element(2, erlang:element(5, SimilarTransition)) ++ "."),
			{ok, Actions} = erl_parse:parse_term(ActionsTokens),
			case erlang:element(1, Actions) of
			channel ->
			    lists:foreach(
			    	fun(Action) ->
				    case erlang:element(1, Action) of
				    global ->
				    	io:format(File, "\t\t\tgen_fsm:send_event("++ System ++"_~w_fsm, ~s),\n", [erlang:element(2, Action), erlang:element(3, Action)]);
				    foreach ->
				    	io:format(File, "\t\t\tlists:foreach(fun(FsmPid) ->\n" ++
					"\t\t\t\tlists:last(FsmPid) ! {'$gen_event', ~s}\n" ++
					"\t\t\tend, ets:match(hashTable, {{'_', ~w}, '$1'})),\n", [erlang:element(3, Action), erlang:element(2, Action)]);
				    parent ->
				    	io:format(File, "\t\t\terlang:element(2, lists:last(ets:lookup(hashTable, {State, ~w}))) ! {'$gen_event', ~s},\n", [erlang:element(2, Action), erlang:element(3, Action)]);
				    _ ->
				    	false
				    end
				end, erlang:element(2, Actions));
			variable ->
			    lists:foreach(
			    	fun(Action) ->
				    io:format(File, "\t\t\t~s,\n", [Action])
			    	end, erlang:element(2, Actions))
			end,
			io:format(File, "~s", ["\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ "~n\", [self()]),\n" ++
			"\t\t\t{next_state, " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ ", State}"]);
		_ ->
		    false
	    end;
	_ ->
	    false
    end,
    action(File, Tail, System, Filename).

transition_with_no_arguments(File, [], System, Filename, State) -> [];
transition_with_no_arguments(File, [SimilarTransition|Tail], System, Filename, State) ->
    case erlang:tuple_size(SimilarTransition) of
	4 ->
	    io:format(File, "~s", ["\t\t\tio:format(\""++Filename++" - ~w - is in state: " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ "\", [self()]),\n" ++ "\t\t\t{next_state, " ++ erlang:atom_to_list(erlang:element(3, SimilarTransition)) ++ ", State}"]);
	_ ->
	    false
    end,
    transition_with_no_arguments(File, Tail, System, Filename, State).

check_condition_plus_action(Flag, []) -> [];
check_condition_plus_action(Flag, [SimilarTransition|Tail]) ->
    case erlang:tuple_size(SimilarTransition) of
	6 -> NewFlag = true;
	_ -> NewFlag = false
    end,
    case NewFlag of
	true ->
	    true;
	false ->
    	    check_condition_plus_action(NewFlag, Tail)
    end.

check_condition(Flag, []) -> [];
check_condition(Flag, [SimilarTransition|Tail]) ->
    case erlang:tuple_size(SimilarTransition) of
	5 -> case erlang:element(1, erlang:element(5, SimilarTransition)) of
		condition ->
			NewFlag = true;
		_ -> 
			NewFlag = false
	     end;

	_ -> NewFlag = false
    end,
    case NewFlag of
	true ->
	    true;
	false ->
    	    check_condition(NewFlag, Tail)
    end.

check_action(Flag, []) -> [];
check_action(Flag, [SimilarTransition|Tail]) ->
    case erlang:tuple_size(SimilarTransition) of
	5 -> case erlang:element(1, erlang:element(5, SimilarTransition)) of
		action ->
			NewFlag = true;
		_ -> 
			NewFlag = false
	     end;

	_ -> NewFlag = false
    end,
    case NewFlag of
	true ->
	    true;
	false ->
    	    check_action(NewFlag, Tail)
    end.
