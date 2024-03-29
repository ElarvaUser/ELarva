-module(doctor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([newDoctor/1,addPatient/2]).

%%
%% API Functions
%%
newDoctor(Name) ->  
    %spawn process which simply triggers login state change of doctor
    logIn(Name),
    loop(Name, loggedIn).
	
loop(Name, LoginState) -> 
    receive
	{req,{Patient,Diagnosis}} -> 
	    io:format("*--- Doctor ~w was notified by backend for a patient request by ~w. ~n",[Name, Patient]),
            loop(Name, LoginState);

	{sendResponse, Patient, Res} ->
	    sendResponse({Patient,fever},Name,Res),
	    io:format("*--- Response sent by ~w for patient - ~w - where response is ~w. ~n",[Name, Patient, Res]),
	    loop(Name, LoginState);

	 {addPatient,Patient} -> 
	    addPatient(Name,Patient),
	    loop(Name, LoginState);

	 {loginStateChange} ->
	    if (LoginState == loggedOut) -> 
		logIn(Name),
 		LoginState2 = loggedIn;		 
    	    (LoginState == loggedIn) -> 
		logOut(Name),
 		LoginState2 = loggedOut
    	    end,
    	    loop(Name, LoginState2)
    end.

sendResponse(PatientRecord,DocName,Res) ->
    backend ! {doc_response,DocName,Res,PatientRecord}.

logIn(DocName) -> 
    backend ! {doctor_login,DocName}.

logOut(DocName) -> 
    backend ! {doctor_logout,DocName}.

addPatient(DocName,Patient) -> 
    backend ! {linkPatientToDoc,DocName,Patient}.

%%
%% Local Functions
%%

%counter(IntValue) ->
%	receive
%		{get,ResponsePid} -> ResponsePid ! {getCounterRes,IntValue},
%							 counter(IntValue);
%		{inc,_} -> counter(IntValue + 1)
%	end.

%getCounterValue(CounterPid) -> 
%	CounterPid ! {get,self()},
%	receive
%	   {getCounterRes,Int} -> Int
%	end.

%incCounterValue(CounterPid) ->
%	CounterPid ! {inc,self()}.

%addTraceElem(Name,CounterPid,Event,EventParams,Trace) -> incCounterValue(CounterPid),
%	                                                     io:format("Doctor ~w generated event ~w with counter value ~w. ~n",[Name,Event,getCounterValue(CounterPid)]),
%												         [{Name,getCounterValue(CounterPid),Event,EventParams}] ++ Trace.  
