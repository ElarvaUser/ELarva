%% Author: Dru
%% Created: 6 Dec 2010
%% Description: TODO: Add description to backend
-module(backend).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([main/0,start_link/0,loop/5]).

-export([newBackend/0,queryDoctor/2,timer/0]).
-export([isOpenPatientRequest/2,
	 getRequestCounterValue/2,
	 removePatientRequest/2,
	 decPatientRequestCounter/2]).

%%
%% API Functions
%%

main() -> main:main().

start_link() ->
    BackendPid = newBackend(),
    {ok, BackendPid}.

newBackend() -> 
    % spawn the backend timer - will be used to trigger the system into checking for pending doctor queries
    %spawn(backend,timer,[]),
    % spawn the backend
    BackendPid = spawn_link(backend,loop,[[],[],[],[],[]]), 
    io:format("*--- Backend started ~n", []),
    % register the backend's monitor manager under the appropriate name
    register(backend,BackendPid),
    BackendPid.

loop(PatientRecords, DoctorPatientLinks, DoctorsOnline, CurrentPatientRequests, WaitingOfflineDoctorRequests) -> 
    receive
% 1.
	{request_record,PatientName} ->
	    % get patient records				   
            PatientRecord = retrieveRecord(PatientName,PatientRecords), 
	    % get the doctors which are responsible for the patient
	    PatientDocs = chooseDoctors(PatientName,DoctorPatientLinks), 
	    % get the doctors which are responsible for the patient that are currently online
	    OnlinePatientDocs = listIntersection(PatientDocs,DoctorsOnline), 
	    % get docs which are currently offline but require permission
	    OfflinePatientDocs = PatientDocs -- DoctorsOnline, 
	    NewWaitingOfflineDoctorRequests = addToDoctorRequests(PatientRecord,OfflinePatientDocs) ++ WaitingOfflineDoctorRequests,
	    % add patient request, together with number of approvals we need to wait for to list of currently open requests
	    NewCurrentPatientRequests = [{PatientName,length(PatientDocs)}|CurrentPatientRequests], 

	    %if (length(OnlinePatientDocs) == 0) ->                                
		% !!! - This is the induced error - here we are stating that if a patient has no doctors, or none of the doctors are online, send the response                  
		%sendResponse(MonitorManagerPid,PatientName,PatientRecord);     % with the patient record immediately. We then argue that this behaviour is unwanted as specified by the system requirements (make this point clear when discussing the system requirements in documentation) 
		%(1 == 1) ->                                                         
		% else someone is online, send queries to all doctors! 

	    % query the patient's doctors that are online for approval.
	    queryDoctors(PatientRecord,OnlinePatientDocs), 
						   %end,
	    loop(PatientRecords,DoctorPatientLinks,DoctorsOnline,NewCurrentPatientRequests,NewWaitingOfflineDoctorRequests);
		
% 2.
	{trigger_query_offline_doctors} -> 
	    % query those doctors that were offline and now are currently online
    	    NewWaitingOfflineDoctorRequests = queryDoctorsWhichOnline(WaitingOfflineDoctorRequests,DoctorsOnline),
    	    loop(PatientRecords,DoctorPatientLinks,DoctorsOnline,CurrentPatientRequests,NewWaitingOfflineDoctorRequests);

% 3.	% get doctor response
	{doc_response,DocName,Res,{Patient,Diagnosis}} -> 
	    % check if request for doctor response present 
	    BoolRes = isOpenPatientRequest(Patient,CurrentPatientRequests), 
	    if (BoolRes == false) ->% if no such request available, ignore
		io:format("!--- Response to unknown request received - {~w,~w,{~w,~w}}. ~n",[DocName,Res,Patient,Diagnosis]),
		loop(PatientRecords,DoctorPatientLinks,DoctorsOnline,CurrentPatientRequests,WaitingOfflineDoctorRequests);
	    (1 == 1) -> % else
		if (Res == reject) -> % if request rejected, simply inform user and discard request info
		    % inform patient that request has been denied
		    sendResponse(Patient),
		    % remove info of request, since request has been given response
		    NewRequestList = removePatientRequest(Patient,CurrentPatientRequests), 
		    % remove any possibly pending requests for that patient.
		    NewWaitingOfflineDoctorRequests = removeFromDoctorRequests(Patient,WaitingOfflineDoctorRequests),
		    % restart loop 
		    loop(PatientRecords,DoctorPatientLinks,DoctorsOnline,NewRequestList,NewWaitingOfflineDoctorRequests);

	    	(1 == 1) -> % else
		    % add received record to PatientRecords
		    NewPatientRecords = [{Patient,Diagnosis}] ++ PatientRecords,
		    % decrement counter of approved requests
	            NewRequestList = decPatientRequestCounter(Patient,CurrentPatientRequests),
		    % get count of positive responses still waiting for 
		    Count = getRequestCounterValue(Patient,NewRequestList), 
		    if (Count == 0) -> % if counter reached zero, can send info
			% send info to patient
			% used to violate property 2
			% sendResponse(Patient,{'Paul',Diagnosis}),
			% generate some kind of delay to allow message sent by backend_doctor_fsm to be received by the patient fsm before the send response of the doctor.
			timer:sleep(1000),
			sendResponse(Patient,{Patient,Diagnosis}),
			% remove patient request info, since request satisfied
		    	NewRequestList2 = removePatientRequest(Patient,NewRequestList), 
			% restart loop
		    	loop(NewPatientRecords, DoctorPatientLinks, DoctorsOnline, NewRequestList2, WaitingOfflineDoctorRequests);
		    
		    % if still didnt reach zero, wait for other responses
 	     	    (1 == 1) -> 
			% restart loop
			io:format("*--- ~w doctor's responses remaining for patient - ~w ~n", [Count,Patient]),
			loop(NewPatientRecords, DoctorPatientLinks, DoctorsOnline, NewRequestList, WaitingOfflineDoctorRequests) 
		    end
	        end	  
	    end;
		
% 4.
	{linkPatientToDoc,Doc,Patient} ->  
	    % add new record to current doctor & patient record information
	    NewDoctorPatientLinks = [{Doc,Patient}|DoctorPatientLinks], 
	    io:format("*--- New Link between ~w and ~w~n", [Doc,Patient]),
	    % restart loop with new info
	    loop(PatientRecords,NewDoctorPatientLinks,DoctorsOnline,CurrentPatientRequests,WaitingOfflineDoctorRequests);

% 5.
	{doctor_login, Doc} -> 
	    % build list of currently online doctors
	    NewDocsOnline = registerDoctorOnline(Doc,DoctorsOnline),
	    % restart loop with new info of doctors online 
	    loop(PatientRecords,DoctorPatientLinks,NewDocsOnline,CurrentPatientRequests,WaitingOfflineDoctorRequests);
		
% 6.
	{doctor_logout,Doc} -> 
	    % build list of currently online doctors				 
	    NewDocsOnline = registerDoctorOffline(Doc,DoctorsOnline),
	    % restart loop with new info of doctors online 
	    loop(PatientRecords,DoctorPatientLinks,NewDocsOnline,CurrentPatientRequests,WaitingOfflineDoctorRequests);

% 7.
	{createPatient, PatientName} ->
	    Pid = spawn(patient,newPatient,[PatientName]),
	    register(PatientName,Pid),
	    %io:format("*--- New Patient with name - ~w~n", [PatientName]),
	    loop(PatientRecords, DoctorPatientLinks, DoctorsOnline, CurrentPatientRequests, WaitingOfflineDoctorRequests);
% 8.
	{createDoctor, DoctorName} ->
	    Pid = spawn(doctor,newDoctor,[DoctorName]),
	    register(DoctorName,Pid),
	    %io:format("*--- New Doctor with name - ~w~n", [DoctorName]),
	    loop(PatientRecords, DoctorPatientLinks, DoctorsOnline, CurrentPatientRequests, WaitingOfflineDoctorRequests);

	_ ->
	    false
    end.

timer() ->
    timer:sleep(5000), % sleep for 5 seconds
    backend ! {trigger_query_offline_doctors}, % trigger the backend to recheck if any doctors have now come online
    timer(). % restart loop

%ok
%receivePatientRecord() ->
%	receive
%		{receivePatientRecord,Doc,PatientRecord} -> Doc2 = Doc,Res = PatientRecord
%	end,
%	Res.

%ok
queryDoctorsWhichOnline(WaitingRequests,[]) -> WaitingRequests;
queryDoctorsWhichOnline([],OnlineDocs) -> [];
queryDoctorsWhichOnline([FirstReq|ReqTail],OnlineDocs) ->
	{Doc,PatientRecord} = FirstReq,
	BoolRes = lists:member(Doc, OnlineDocs),
	if (BoolRes == true) ->
		 queryDoctor(PatientRecord,Doc),
		 queryDoctorsWhichOnline(ReqTail,OnlineDocs);
	   (1 == 1) ->
		 [{Doc,PatientRecord}] ++ queryDoctorsWhichOnline(ReqTail,OnlineDocs)
	end.

%ok
addToDoctorRequests(PatientRecord,[]) -> [];
addToDoctorRequests(PatientRecord,[Head|Tail]) ->
	[{Head,PatientRecord}|addToDoctorRequests(PatientRecord,Tail)].

removeFromDoctorRequests(Patient,[]) -> [];
removeFromDoctorRequests(Patient,[Head|Tail]) ->
	{Doc,{Patient2,_}} = Head,
	if (Patient == Patient2) -> removeFromDoctorRequests(Patient,Tail);
	   (1 == 1) -> [Head|removeFromDoctorRequests(Patient,Tail)]
	end.

%ok
isOpenPatientRequest(Patient,[]) -> false;
isOpenPatientRequest(Patient,[Head|Tail]) ->
	{PatientName,_} = Head,
	if (Patient == PatientName) -> true;
	   (1 == 1) -> isOpenPatientRequest(Patient,Tail)
	end.

%ok
getRequestCounterValue(Patient,[]) -> -1;
getRequestCounterValue(Patient,[Head|Tail]) ->
	{PatientName,Counter} = Head,
	if (Patient == PatientName) -> Counter;
	   (1 == 1) -> getRequestCounterValue(Patient,Tail)
	end.

%ok
removePatientRequest(Patient,[]) -> [];
removePatientRequest(Patient,[Head|Tail]) ->
	{PatientName,_} = Head,
	if (Patient == PatientName) -> Tail;
	   (1 == 1) -> [Head|removePatientRequest(Patient,Tail)]
	end.

%ok
decPatientRequestCounter(Patient,[]) -> [];
decPatientRequestCounter(Patient,[Head|Tail]) ->
	{PatientName,Counter} = Head,
	if (Patient == PatientName) -> [{PatientName,Counter-1}|Tail];
	   (1 == 1) -> [Head|decPatientRequestCounter(Patient,Tail)]
	end.

%ok
%linkPatientToDoctor() ->
%	receive
%	   {linkPatientToDoc,Doc,Patient} -> Res = {Doc,Patient}
%	end,
%	Res.

%ok
queryDoctor(PatientRecord,DocName) ->
    DocName ! {req,PatientRecord}.

%ok
queryDoctors(PatientRecord,[]) -> ok;
queryDoctors(PatientRecord,[Head|Tail]) -> queryDoctor(PatientRecord,Head),
												 queryDoctors(PatientRecord,Tail).

%ok
%receiveRequest() ->
%	receive
%		{request_record,Name} -> Name % receive request from patient
%	end,
%	main:delay(1,3), % delay for a few seconds to mirror duration in communication - gives time to monitors to migrate etc...
%	Name.

%ok
retrieveRecord(Patient,[]) ->
	{Patient,no_diagnosis};
retrieveRecord(Patient,[Head|Tail]) ->
	{Patient2,Diagnosis} = Head,
	if (Patient == Patient2) -> 
	    {Patient,Diagnosis};
	    (1 == 1) -> retrieveRecord(Patient,Tail)
	end.
	
%ok
registerDoctorOnline(Doc,OnlineDoctors) ->
    [Doc|OnlineDoctors].

%ok
registerDoctorOffline(Doc,[]) ->
	[];
registerDoctorOffline(Doc,[Head|Tail]) ->
	if (Doc == Head) -> 
	    Tail;
	    (1 == 1) -> registerDoctorOffline(Doc,Tail)
	end.

%ok
sendResponse(Patient) -> 
	Patient ! {backend_response,request_denied}.

%ok
sendResponse(Patient,PatientRecord) ->
	Patient ! {backend_response,PatientRecord}.
     
linkPatientToDoctor(Patient,Doctor,Diagnosis,DoctorPatientRecords) ->
	[{Doctor,{Patient,Diagnosis}}|DoctorPatientRecords].

%ok
chooseDoctors(Patient,DoctorPatientLink) ->
	extractDocs(Patient,DoctorPatientLink).
	
%ok
%getDoctorResponse() ->
%	receive
%		{doc_response,DocName,Res,PatientRecord} -> DocName2 = DocName,Res2 = Res,PatientRecord2 = PatientRecord
%	end,
%	{DocName2,Res2,PatientRecord}.



isPatientsDoctor(Patient,Doc,[]) ->
	false;
isPatientsDoctor(Patient,Doc,[Head|Tail]) ->
	{Doc2,{Patient2,_}} = Head,
	if ((Doc2 == Doc) and (Patient2 == Patient)) -> true;
	   (1==1) -> isPatientsDoctor(Patient,Doc,Tail)
	end.


	

%%
%% Local Functions
%%

% ok
listIntersection(List1,List2) ->
	Set1 = sets:from_list(List1),
	Set2 = sets:from_list(List2),
	SetRes = sets:intersection(Set1, Set2),
	sets:to_list(SetRes).

%ok
extractDocs(Patient,[]) -> [];
extractDocs(Patient,[Head|Tail]) ->
	{Doc,Patient2} = Head,
	if (Patient2 == Patient) -> [Doc] ++ extractDocs(Patient,Tail);
	   (1==1) -> extractDocs(Patient,Tail)
	end.
