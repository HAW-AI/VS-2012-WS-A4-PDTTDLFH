-module(coordinator).

-behaviour(gen_server).

%% API
-export([easy_start/0, start/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {team_number,
                station_number,
                current_slot,
                receiver_pid :: pid(),
                sender_pid :: pid(),
                slot_wishes,
                used_slots,
                own_packet_collided}).

easy_start() ->
  start([15100, 8, 99, '225.10.1.2', '141.22.27.102']).

start([ReceivingPort, TeamNumber, StationNumber, MulticastIP, LocalIP]) ->
  SendingPort = 14000 + TeamNumber,
  gen_server:start(?MODULE,
                   [ReceivingPort, SendingPort, TeamNumber, StationNumber,
                    MulticastIP, LocalIP], []);
start([ReceivingPort, SendingPort, TeamNumber, StationNumber, MulticastIP, LocalIP]) ->
  gen_server:start(?MODULE,
                   [ReceivingPort, SendingPort, TeamNumber, StationNumber,
                    MulticastIP, LocalIP], []).

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
                                      {multicast_loop, true},
                                      {ip, ParsedLocalIP}
                                     ]),
  gen_udp:send(SendingSocket, ParsedMulticastIP, ReceivingPort, <<"111111111111111111111111111111111">>),

  utility:log("sending socket ~p~n",[inet:port(SendingSocket)]),
  %%% start the receiver and sender processes
  {ok, ReceiverPID} = receiver:start(self(), ReceivingSocket),
  {ok, SenderPID}   =   sender:start(self(),
                                     SendingSocket,
                                     ParsedMulticastIP,
                                     ReceivingPort),
  gen_udp:controlling_process(ReceivingSocket,ReceiverPID),
  gen_udp:controlling_process(SendingSocket,SenderPID),
  %%% start timer for first sending round
  create_prepare_sending_timer(),
									 
  {ok, #state{team_number         = TeamNumber,       %
              station_number      = StationNumber,    % HOSTNAME##lab
              current_slot        = get_random_slot(),% the slot we are trying to send in
              receiver_pid        = ReceiverPID,      % PID of the receiver gen_server
              sender_pid          = SenderPID,        % PID of the sender gen_server
              slot_wishes         = dict:new(),       % [{SlotNumber,[Station1, Station2]}, ...]
              used_slots          = [],               % list of all slots in use. determined by seen packets
              own_packet_collided = false             % 
             }}.
			 
handle_cast(prepare_sending, State) ->
	%start timer for next sending round
	utility:log("lets start a new round"),
	create_prepare_sending_timer(),
	NewCurrentSlot = case State#state.own_packet_collided of
		true ->
			calculate_free_slot(State#state.slot_wishes);
		false ->
			State#state.current_slot
	end,
	gen_fsm:send_event(State#state.sender_pid, {slot, NewCurrentSlot}),
	%resetting state for next round except for the new slot
	{noreply, State#state{current_slot = NewCurrentSlot, slot_wishes = dict:new(), used_slots=[], own_packet_collided = false}};

%%% async incoming messages
handle_cast({received, Slot, _Time, Packet}, State) ->
  utility:log("received something"),
  NewSlotwishes = register_slotwishes(Packet, State#state.slot_wishes),
  case check_for_packet_collision(Slot, State) of
    true ->
      % check if the Slot of the packet is the same that we are sending in
      case Slot == State#state.current_slot of
        true ->
          % Add slot to Slotwishes again so that it will be sorted out in the
          % next round when we are checking for a free slot
          SlotwishesWithCollision = dict:append(Slot,
                                                State#state.station_number,
                                                NewSlotwishes),
          % TODO log own packet collision
          {noreply, State#state{own_packet_collided = true,
                                slot_wishes         = SlotwishesWithCollision}};
        false ->
          {noreply, State#state{own_packet_collided = false,
                                slot_wishes         = NewSlotwishes}}
      end;
    false -> % no collision at all
      UsedSlots = [Slot | State#state.used_slots],
      {noreply, State#state{slot_wishes = NewSlotwishes,
                            used_slots  = UsedSlots}}
  end;
 
handle_cast({revise_next_slot, CurrentNextSlot}, State) ->
	utility:log("coordinator: revising current next slot"),
	case dict:is_key(CurrentNextSlot, State#state.slot_wishes) of
		true -> %wish for slot, choose another
			utility:log("coordinator: invalid - calculating new slot"),
			NewNextSlot = calculate_free_slot(State#state.slot_wishes),
			gen_fsm:send_event(State#state.sender_pid, {next_slot, NewNextSlot}),
			{noreply, State#state{current_slot = NewNextSlot}};
		false -> %still free, keep it
			utility:log("coordinator: valid - just go on"),
			gen_fsm:send_event(State#state.sender_pid, {next_slot, CurrentNextSlot}),
			{noreply, State}
	end;

handle_cast(kill, State) ->
  {stop, normal, State};

handle_cast(_UnknownMessage, State) ->
  % TODO log UnknownMessage
	utility:log("unknown msg"),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, State) ->
  gen_server:cast(State#state.receiver_pid, kill),
  gen_fsm:send_event(State#state.sender_pid, kill),
  ok.

%%%%% Helpers
create_prepare_sending_timer() ->
utility:log("creating timer"),
	erlang:send_after(1000 - (utility:current_timestamp() rem 1000),self(),prepare_sending).

get_random_slot() ->
	utility:log("getting random slot"),
  random:uniform(20) - 1.

calculate_free_slot(Slotwishes) ->
	utility:log("calc free slot"),
  NonCollisionSlots = dict:filter(
    %%% Valid slots just have one sender.
    fun(_,V) ->
      length(V) == 1 %%% V = list of stations
    end,
    Slotwishes
  ),
  AvailableSlots = lists:subtract(lists:seq(0,19),
                                  dict:fetch_keys(NonCollisionSlots)),
  case length(AvailableSlots) == 0 of
	true ->
	  get_random_slot();
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
  {StationNumber, Slot, _, _} = parse_message(Packet),
  dict:append(Slot, StationNumber, Slotwishes).

%%% TODO handle malformed packets
parse_message(Packet) ->
  <<_StationIdentifier:8/binary,
    StationNumber:16/integer-big,
    PayloadBin:14/binary,
    Slot:8/integer-big,
    Timestamp:64/integer-big
  >> = Packet,
  Payload = binary_to_list(PayloadBin),
  {StationNumber, Slot, Payload, Timestamp}.

handle_info(prepare_sending, State) ->
  gen_server:cast(self(), prepare_sending),
  {noreply, State};

%%% OTP gen_server boilerplate - ignore this
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

