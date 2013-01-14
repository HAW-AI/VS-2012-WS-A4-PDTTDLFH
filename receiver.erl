-module(receiver).

-behaviour(gen_server).

%% API
-export([start/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {coordinator_pid :: pid(),
                receiving_socket,
				own_ip,
				own_sending_port}).

start(CoordinatorPID, ReceivingSocket, LocalIp, SendingPort) ->
  gen_server:start(?MODULE, [CoordinatorPID, ReceivingSocket, LocalIp, SendingPort], []).

init([CoordinatorPID, ReceivingSocket, LocalIp, SendingPort]) ->
  {ok, #state{coordinator_pid  = CoordinatorPID, receiving_socket = ReceivingSocket, own_ip = LocalIp, own_sending_port = SendingPort}}.

handle_cast(kill, State) ->
  utility:log("receiver: received kill message"),
  {stop, normal, State}; % calls :terminate and then shuts down

handle_cast(UnknownMessage, State) ->
  utility:log("received unknown msg: ~p~n", [UnknownMessage]),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, State) ->
  utility:log("receiver: closed receiving socket"),
  gen_udp:close(State#state.receiving_socket),
  utility:log("receiver: terminated"),
  ok.

handle_info({udp, _Socket, IPtuple, InPortNo, Packet}, State) ->
  case (inet_parse:ntoa(IPtuple) == atom_to_list(State#state.own_ip)) and (InPortNo == State#state.own_sending_port) of
    true ->
	  utility:log("receiver: received own packet");
    false ->
	  Timestamp = utility:current_timestamp(),
	  Slot      = utility:slot_of_timestamp(Timestamp),
	  utility:log("receiver: received packet~p~n",[IPtuple]),
	  utility:log("seams to be in slot ~p~n",[Slot]),
	  gen_server:cast(State#state.coordinator_pid, {received, Slot, Timestamp, Packet})
  end,
  {noreply, State};
  
%%% OTP gen_server boilerplate - ignore this
handle_info(Info, State) ->
  utility:log("receiver: how about sending gen server a msg in a proper way: ~p~n",[Info]),
  {noreply, State}.
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
