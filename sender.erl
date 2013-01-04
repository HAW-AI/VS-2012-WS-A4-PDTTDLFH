-module(sender).

-behaviour(gen_fsm).

%% API
-export([start/4]).

%% gen_fsm callbacks
-export([init/1,
         waiting_for_slot/2,
         waiting_for_input/2,
         send_message/2,
         state_name/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {datasource_pid :: pid(), % PID of the datasource gen_server
                sending_socket,          %
                multicast_ip,            % IP used for broadcast
                receiving_port,          %
                coordinator_pid :: pid(),% PID of the coordinator gen_server
                slot                     % slot of each frame used for sending
               }).

start(CoordinatorPID, SendingSocket, MulticastIP, ReceivingPort) ->
  gen_fsm:start(?MODULE, [CoordinatorPID,
                          SendingSocket,
                          MulticastIP,
                          ReceivingPort], []).

init([CoordinatorPID, SendingSocket, MulticastIP, ReceivingPort]) ->
  gen_udp:controlling_process(SendingSocket, self()),
  {ok, DataSourcePID} = datasource:start(),
  {ok, wait_for_slot, #state{datasource_pid  = DataSourcePID,
                             sending_socket  = SendingSocket,
                             multicast_ip    = MulticastIP,
                             receiving_port  = ReceivingPort,
                             coordinator_pid = CoordinatorPID
                            }}.

waiting_for_slot({slot, Slot}, State) ->
  io:format("waiting_for_slot: {slot, ~p}~n", [Slot]),
  {next_state, waiting_for_input, State#state{slot = Slot}};
waiting_for_slot(Event, State) ->
  io:format("waiting_for_slot: unknown event: ~p~n", [Event]),
  {next_state, waiting_for_slot, State}.

waiting_for_input({input, Data}, State) ->
  io:format("waiting_for_input: {input, ~p}~n", [Data]),
  %%% do something
  {next_state, send_message, State};
waiting_for_input(Event, State) ->
  io:format("waiting_for_input: unknown event: ~p~n", [Event]),
  {next_state, waiting_for_input, State}.

send_message(Foobar, State) ->
  Packet = build_packet(), % TODO

  gen_udp:send(State#state.sending_socket,
               State#state.multicast_ip,
               State#state.receiving_port,
               Packet),
  {next_state, waiting_for_slot, State}.

handle_event(kill, _StateName, State) ->
  {stop, normal, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, _StateName, State) ->
  gen_server:cast(State#state.datasource_pid, kill),
  gen_udp:close(State#state.sending_socket),
  ok.




%%% Helper functions
build_packet() ->
  <<"The Datapacket">>.


%%% OTP gen_fsm boilerplate - ignore this
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
