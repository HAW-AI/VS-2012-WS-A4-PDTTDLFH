-module(utility).
-compile(export_all).

-define(NUM_SLOTS, 20).
-define(SLOT_TIME, 50). % in milliseconds

% timestamp in millisec since 00:00 UTC, January 1, 1970
current_timestamp() ->
  {Mega, Sec, Micro} = now(),
  Mega * 1000000000 + Sec * 1000 + Micro div 1000.

current_frame() ->
	{_, CurrentFrame, _} = now(),
	CurrentFrame.

 % Timestamp rem 1000 => ms part of timestamp
 % ms part / 50 => the slot cause 1000 ms / 50 => 20 slots
slot_of_timestamp(Timestamp) ->
	trunc(((Timestamp rem 1000) / 50)).

% time in milliseconds until slot in _next_ frame
time_until_slot(Slot) ->
  CurrentTime = current_timestamp(),
  CurrentSlot = slot_of_timestamp(CurrentTime),
  ElapsedTime = CurrentTime rem ?SLOT_TIME,     % elapsed time since beginning of current slot
  (?NUM_SLOTS - (CurrentSlot - Slot)) * ?SLOT_TIME - ElapsedTime + ?SLOT_TIME div 2.