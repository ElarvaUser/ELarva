%% Author: Dru
%% Created: 15 Dec 2010
%% Description: TODO: Add description to channel
-module(channel).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([channel/3,newChannel/1,newChannels/1,readChannel/1,writeChannel/2]).

%%
%% API Functions
%%

channel(Name,Queue,WaitingPids) ->
	receive
		{questionMark,ResPid} -> BoolRes = queue:is_empty(Queue),
			                     if (BoolRes == true) -> NewWaitingPids = [ResPid|WaitingPids],
										                 channel(Name,Queue,NewWaitingPids); 
									(1 == 1) ->  Head = queue:head(Queue),
												 Tail = queue:tail(Queue),
										         ResPid ! {channelRead,Name,Head},
								                 channel(Name,Tail,WaitingPids)
								 end;
		{pling,Val} -> WaitingPidsLength = length(WaitingPids),
					   if (WaitingPidsLength == 0) -> NewQueue = queue:in(Val, Queue),
							                          channel(Name,NewQueue,WaitingPids);
                          (1 == 1) -> [Head|Tail] = WaitingPids,
									  Head ! {channelRead,Name,Val},
									  channel(Name,Queue,Tail)
					   end
	end.

newChannels([]) -> ok;
newChannels([Head|Tail]) ->
	newChannel(Head),
	newChannels(Tail).

newChannel(Name) ->
	Queue = queue:new(),
	Pid = spawn(channel,channel,[Name,Queue,[]]),
    register(Name,Pid).

writeChannel(Name,Val) ->
	%io:format("arghh. ~n"),
	Name ! {pling,Val}.

readChannel(Name) ->
	Name ! {questionMark,self()},
	receive
		{channelRead,Name,Val} -> Res = Val
	end,
	Res.

%%
%% Local Functions
%%

