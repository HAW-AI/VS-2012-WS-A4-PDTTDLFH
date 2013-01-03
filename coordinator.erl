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

-record(state, {team_number,
                station_number,
                current_slot,
                receiver_pid :: pid(),
                sender_pid :: pid(),
                slot_wishes,
                used_slots,
                own_packet_collided}).

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
                                        {multicast_loop, false},
                                        {add_membership, {ParsedMulticastIP,
                                                          ParsedLocalIP}}
                                       ]),
  {ok, SendingSocket} = gen_udp:open(SendingPort,
                                     [binary,
                                      {active, true},
                                      {ip, ParsedLocalIP},
                                      inet,
                                      {multicast_loop, false},
                                      {multicast_if, ParsedLocalIP}
                                     ]),

  %%% start the receiver and sender processes
  {ok, ReceiverPID} = receiver:start(self(), ReceivingSocket),
  {ok, SenderPID}   =   sender:start(self(),
                                     SendingSocket,
                                     ParsedMulticastIP,
                                     ReceivingPort),

  {ok, #state{team_number         = TeamNumber,
              station_number      = StationNumber,
              current_slot        = get_random_slot(),
              receiver_pid        = ReceiverPID,
              sender_pid          = SenderPID,
              slot_wishes         = dict:new(),
              used_slots          = [],
              own_packet_collided = false
             }}.

%%% async incoming messages
handle_cast({received, _Slot, _Time, _Packet}, State) ->
  %%% do something with the received data
  {noreply, State};

handle_cast(kill, State) ->
  gen_server:cast(State#state.receiver_pid, kill),
  gen_server:cast(State#state.sender_pid, kill),
  exit(normal),

  {noreply, State}.


%%%%% Helpers
get_random_slot() ->
  random:uniform(20) - 1.






%%% OTP gen_server boilerplate - ignore this
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

