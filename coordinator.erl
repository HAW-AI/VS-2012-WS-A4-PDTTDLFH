-module(coordinator).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define (FRAMES_TO_SKIP, 3).

-record(state, {team_number,
                station_number,
                current_slot,
                receiver_pid :: pid(),
                sender_pid :: pid(),
		datasink_pid :: pid(),
                slot_wishes,
                used_slots,
                needs_new_slot,
                frame_timer}).

start([ReceivingPortAtom, TeamNumberAtom, StationNumberAtom, MulticastIPAtom, LocalIPAtom]) ->
  SendingPort = 14000 + atom_to_integer(TeamNumberAtom),
  gen_server:start(?MODULE,
                  [atom_to_integer(ReceivingPortAtom),
                   SendingPort,
                   atom_to_list(TeamNumberAtom),
                   atom_to_list(StationNumberAtom),
                   MulticastIPAtom,
                   LocalIPAtom],
                  []);
start([ReceivingPortAtom, SendingPortAtom, TeamNumberAtom, StationNumberAtom, MulticastIPAtom, LocalIPAtom]) ->
  gen_server:start(?MODULE,
                  [atom_to_integer(ReceivingPortAtom),
                   atom_to_integer(SendingPortAtom),
                   atom_to_list(TeamNumberAtom),
                   atom_to_list(StationNumberAtom),
                   MulticastIPAtom,
                   LocalIPAtom],
                  []).

init([ReceivingPort, SendingPort, TeamNumber, StationNumber, MulticastIP, LocalIP]) ->
  %%% seed process' random number generator
  random:seed(now()),

  %%% parse IPs into usable format
  {ok, ParsedMulticastIP} = inet_parse:address(atom_to_list(MulticastIP)),
  {ok, ParsedLocalIP} = inet_parse:address(atom_to_list(LocalIP)),

  %%% Open Multicast sockets

  {ok, ReceivingSocket} = gen_udp:open(ReceivingPort,
                                       [binary,
                                        {active, true},
                                        {multicast_if, ParsedLocalIP},
                                        inet,
                                        {reuseaddr, true}, % reuse open port on local machine
                                        {multicast_loop, true},
                                        {add_membership, {ParsedMulticastIP,
                                                          ParsedLocalIP}}
                                       ]),
  utility:log("receiving socket ~p~n",[inet:port(ReceivingSocket)]),
  utility:log("multicast ip ~p~n",[ParsedMulticastIP]),
  utility:log("local ip ~p~n",[ParsedLocalIP]),
  {ok, SendingSocket} = gen_udp:open(SendingPort,
                                     [binary,
                                      {active, true},
                                      {multicast_if, ParsedLocalIP},
                                      inet,
                                      {reuseaddr, true}, % reuse open port on local machine
                                      {multicast_loop, true},
                                      {ip, ParsedLocalIP}
                                     ]),
									 
  utility:log("sending socket ~p~n",[inet:port(SendingSocket)]),
  %%% start the receiver and sender processes
  {ok, ReceiverPID} = receiver:start(self(), ReceivingSocket, LocalIP, SendingPort),
  {ok, SenderPID}   =   sender:start(self(),
                                     SendingSocket,
                                     ParsedMulticastIP,
                                     ReceivingPort),
  gen_udp:controlling_process(ReceivingSocket,ReceiverPID),
  gen_udp:controlling_process(SendingSocket,SenderPID),
  
  {ok, DataSinkPID} = datasink:start(TeamNumber, StationNumber),
  
  %%% start timer for first sending round
  Timer = create_msg_timer(2000, first_frame),
  {ok, #state{team_number         = TeamNumber,       %
              station_number      = StationNumber,    % HOSTNAME##lab
              receiver_pid        = ReceiverPID,      % PID of the receiver gen_server
              sender_pid          = SenderPID,        % PID of the sender gen_server
			  datasink_pid        = DataSinkPID,      % PID of the datasink gen_server
              slot_wishes         = dict:new(),       % [{SlotNumber,[Station1, Station2]}, ...]
              used_slots          = [],               % list of all slots in use. determined by seen packets
              needs_new_slot = false,             %
              frame_timer = Timer
             }}.

%%% async incoming messages
handle_cast({received, Slot, TimestampReceived, Packet}, State) ->
  utility:log("received something"),
  NewSlotwishes = register_slotwishes(Packet, State#state.slot_wishes),
  case check_for_packet_collision(Slot, State) of
    true ->
      utility:log("collision detected"),
      % check if the Slot of the packet is the same that we are sending in
      case Slot == State#state.current_slot of
        true ->
          utility:log("collision with own packet"),
          % Add slot to Slotwishes again so that it will be sorted out in the
          % next round when we are checking for a free slot
          NewTimer = case State#state.needs_new_slot of
            true ->
              State#state.frame_timer;
            false ->
              %skip up to 2 frames to avoid to be stuck in collisions on same slots repeatedly
	      FramesToSkip = random:uniform(?FRAMES_TO_SKIP),
	      restart_msg_timer(1000 * FramesToSkip, new_frame, State#state.frame_timer)
          end,
          {noreply, State#state{needs_new_slot = true,
                                slot_wishes    = NewSlotwishes,
                                frame_timer    = NewTimer}};
        false ->
          {noreply, State#state{slot_wishes         = NewSlotwishes}}
      end;
    false -> % no collision at all
      {StationIdentifier, StationNumber, NextSlot, Payload, Timestamp} = parse_message(Packet),
      gen_server:cast(State#state.datasink_pid, {write_data, StationIdentifier, StationNumber, NextSlot, Payload, Timestamp, TimestampReceived}),
      UsedSlots = [Slot | State#state.used_slots],
      {noreply, State#state{slot_wishes = NewSlotwishes,
                            used_slots  = UsedSlots}}
  end;

handle_cast(needs_new_slot, State) ->
  utility:log("needs new slot"),
  {noreply, State#state{needs_new_slot = true}};

handle_cast(kill, State) ->
  {stop, normal, State};

handle_cast(_UnknownMessage, State) ->
  utility:log("unknown msg"),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, State) ->
  exit(State#state.datasink_pid, kill),
  gen_server:cast(State#state.receiver_pid, kill),
  gen_fsm:send_event(State#state.sender_pid, kill),
  ok.

%%%%% Helpers
atom_to_integer(Atom) ->
  list_to_integer(atom_to_list(Atom)).

create_msg_timer(Time, Msg) ->
  utility:log("creating timer"),
  erlang:send_after(Time - (utility:current_timestamp() rem 1000),self(),Msg).

restart_msg_timer(Time, Msg, OldTimerRef) ->
  erlang:cancel_timer(OldTimerRef),
  create_msg_timer(Time, Msg).


calculate_free_slot(Slotwishes) ->
  utility:log("calc free slot"),
  AvailableSlots = lists:subtract(lists:seq(0,19), dict:fetch_keys(Slotwishes)),
  case length(AvailableSlots) == 0 of
	true ->
	  no_free_slot;
	false ->
	  RandomElementIndex = random:uniform(length(AvailableSlots)),
	  lists:nth(RandomElementIndex, AvailableSlots)
   end.

%%% returns true when the Slot collided else false.
-spec check_for_packet_collision(Slot :: integer(), #state{}) -> true | false.
check_for_packet_collision(Slot, State) ->
  lists:member(Slot, State#state.used_slots) orelse Slot == State#state.current_slot.

%%% adds the slot a packet was sent in to the list of used slots
-spec register_slotwishes(Packet :: binary(), Slotwishes :: dict()) -> dict().
register_slotwishes(Packet, Slotwishes) ->
  {_, StationNumber, NextSlot, _, _} = parse_message(Packet),
  dict:store(NextSlot, StationNumber, Slotwishes).

%%% TODO handle malformed packets
parse_message(Packet) ->
  <<StationIdentifierBin:8/binary,
    StationNumberBin:2/binary,
    PayloadBin:14/binary,
    Slot:8/integer-big,
    Timestamp:64/integer-big
  >> = Packet,
  StationIdentifier = binary_to_list(StationIdentifierBin),
  StationNumber = list_to_integer(binary_to_list(StationNumberBin)),
  Payload = binary_to_list(PayloadBin),
  {StationIdentifier, StationNumber, Slot, Payload, Timestamp}.

handle_info(first_frame, State) ->
	utility:log("lets start a new round"),
	case calculate_free_slot(State#state.slot_wishes) of
	  no_free_slot ->
	  utility:log("no free slot. skipping this frame"),
	  NewTimer = create_msg_timer(1000, first_frame),
        {noreply, State#state{slot_wishes = dict:new(), used_slots=[], needs_new_slot = false, frame_timer = NewTimer}};
	  Slot ->
	    utility:log("found free slot. starting first frame"),
	    NewTimer = create_msg_timer(1000, new_frame),
	    gen_fsm:send_event(State#state.sender_pid, {slot, Slot}),
		{noreply, State#state{current_slot = Slot, slot_wishes = dict:new(), used_slots=[], needs_new_slot = false, frame_timer = NewTimer}}
	end;
  
handle_info(new_frame, State) ->
	%start timer for next sending round
	utility:log("lets start a new round"),
        NewTimer = create_msg_timer(1000, new_frame),
	NewCurrentSlot = case State#state.needs_new_slot of
		true ->
		    	utility:log("own packet collided - calculating new slot"),
			case calculate_free_slot(State#state.slot_wishes) of
				no_free_slot ->
					utility:log("send_message: no free slot. skipping this frame!"),
					no_free_slot;
				FreeSlot ->
					gen_fsm:send_event(State#state.sender_pid, {slot, FreeSlot}),
					FreeSlot
			end;
		false ->
			gen_fsm:send_event(State#state.sender_pid, {slot, State#state.current_slot}),
			State#state.current_slot
	end,
	NewState = case NewCurrentSlot of
		no_free_slot ->
			%resetting wishes and used_slots for next frame, keep current slot but get a new one next round
			State#state{slot_wishes = dict:new(), used_slots=[], needs_new_slot = true, frame_timer = NewTimer};	
		_ ->
			%resetting wishes and used_slots for next frame and set the next slot
			State#state{current_slot = NewCurrentSlot, slot_wishes = dict:new(), used_slots=[], needs_new_slot = false, frame_timer = NewTimer}
	end,
	{noreply, NewState};

handle_info({revise_next_slot, CurrentNextSlot}, State) ->
	utility:log("coordinator: revising current next slot"),
	case dict:is_key(CurrentNextSlot, State#state.slot_wishes) of
		true -> %wish for slot, choose another
			utility:log("coordinator: invalid - calculating new slot"),
			NextSlot = case calculate_free_slot(State#state.slot_wishes) of
              			no_free_slot ->
			    		gen_fsm:send_event(State#state.sender_pid, no_free_slot),
					CurrentNextSlot;
				Slot ->
			    		gen_fsm:send_event(State#state.sender_pid, {next_slot, Slot}),
					Slot
			end,
			utility:log("station: ~p~n before: ~p~n after: ~p~n wishes: ~p~n",[State#state.station_number,CurrentNextSlot,NextSlot,dict:to_list(State#state.slot_wishes)]),
            		{noreply, State#state{current_slot = NextSlot}};
		false -> %still free, keep it
			utility:log("coordinator: valid - just go on"),
			gen_fsm:send_event(State#state.sender_pid, {next_slot, CurrentNextSlot}),
			{noreply, State}
	end;

%%% OTP gen_server boilerplate - ignore this
handle_info(Info, State) ->
  utility:log("coordinator: how about sending gen server a msg in a proper way: ~p~n",[Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

