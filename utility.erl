-module(utility).
-compile(export_all).

% timestamp in millisec since 00:00 UTC, January 1, 1970
current_timestamp() -> 
    {Megasec,Sec,Microsec} = erlang:now(),
	(Megasec * 1000000) + Sec + (Microsec / 1000000) div 1000.

current_frame()->
	{_,CurrentFrame,_}=erlang:now(),
	CurrentFrame.	
	
 % Timestamp rem 1000 => ms part of timestamp
 % ms part / 50 => the slot cause 1000 ms / 50 => 20 slots	
slot_of_timestamp(Timestamp)->
	erlang:trunc(((Timestamp rem 1000)/50)).