-module(utility).
-export([current_timestamp/0, current_frame/0, slot_of_timestamp/1,
         time_until_slot/2, log/1, log/2]).

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
	trunc(((Timestamp rem 1000) / ?SLOT_TIME)).

% time in milliseconds until slot in _next_ frame
time_until_slot(Slot, Adjustment) ->
  CurrentTime = current_timestamp(),
  CurrentSlot = slot_of_timestamp(CurrentTime),
  ElapsedTime = CurrentTime rem ?SLOT_TIME,     % elapsed time since beginning of current slot

  % (?NUM_SLOTS - (CurrentSlot - Slot)): distance to desired slot in next frame (in number of slots)
  % <distance to desired slot> * <time of slot> - <already elapsed time> + <time to be in middle of slot>
  ((?NUM_SLOTS - (CurrentSlot - Slot)) * ?SLOT_TIME - ElapsedTime + ?SLOT_TIME div 2) - 1000 - Adjustment.

log(Message) ->
  LogMessage = lists:concat([werkzeug:timeMilliSecond(),
                             " - ",
                             Message,
                             io_lib:nl()]),
  werkzeug:logging(lists:concat(["log/", net_adm:localhost(), ".log"]), LogMessage).

log(Format, Args) ->
  werkzeug:logging(lists:concat(["log/", net_adm:localhost(), ".log"]), io_lib:format(Format, Args)).
