-module(patient).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([newPatient/1]).

%%
%% API Functions
%%

newPatient(Name) -> 
    loop(Name).
	
	                %CounterPid = spawn(patient,counter,[0]), % Create a process encoding the storage of the counter for the particular node. The linking of the 
					                                         % process with the particular node is done through the registration of the names, where process A
					                                         % mimicking the counter for node B will be named as name(B)++"Counter".
					%register(main:createAtomWithSuffix(Name,"Counter"),CounterPid), % register process with appropriate counter name for use later for setC 
					                                                                % command.
					%loop(Name,CounterPid,[]). % start loop which starts patient execution.
	                

%%
%% Local Functions
%%

% the patient loop (i) sends a request, (ii) recieves a response, (iii) restarts the process
loop(Name) -> 
    receive
	{requestRecord} -> 
            % used to violate property 1 
            requestRecord(Name),
	    loop(Name);

	{receiveResponse} ->
            receiveResponse(Name)
    end.
    %loop(Name). 

% Recieve a response from the backend. 
receiveResponse(Name) -> 
	io:format("************Pid: ~w~n", [erlang:self()]),
	receive
		{backend_response,request_denied} -> io:format("*--- Request by ~w was denied. ~n",[Name]);
													
		{backend_response,{Patient,Diagnosis}} -> io:format("*--- Patient record {~w,~w} was recieved by ~w. ~n",[Patient,Diagnosis,Name]) 
	end.

% The patient which requests his patient record. The request is first delayed, then a request to the backend is sent, and finally a trace element of the event is
% also added to the node trace.
requestRecord(Name) -> 
    io:format("*--- Patient request was sent by ~w. ~n", [Name]),
    backend ! {request_record,Name}.

% A counter function, which recieves either a get or an inc command. Function is implemented using tail recursion, in order to keep the counter state.
% If the command recieved is a get, simply send the counter value to the response pid sent as part of the query package. If command is to increment, 
% simply increment by 1 when call for tail recursion.
%counter(IntValue) -> 
%	receive
		%{get,ResponsePid} -> ResponsePid ! {getCounterRes,IntValue},
		%					 counter(IntValue);
		%{inc,_} -> counter(IntValue + 1)
%	end.


% A function which sends a query to the counter process 
%getCounterValue(CounterPid) -> 
%	CounterPid ! {get,self()},
%	receive
%	   {getCounterRes,Int} -> Int
%	end.

% A function which triggers an increment at the counter process
%incCounterValue(CounterPid) ->
%	CounterPid ! {inc,self()}.

%	                                          incCounterValue(CounterPid),
%	                                          io:format("Patient ~w generated event ~w with counter value ~w. ~n",[Name,Event,getCounterValue(CounterPid)]),
%											  [{Name,getCounterValue(CounterPid),Event,EventParams}] ++ Trace.  
	
	
												   
