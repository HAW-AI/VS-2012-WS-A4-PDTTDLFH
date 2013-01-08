-module(receiver).

-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {coordinator_pid :: pid(),
                receiving_socket}).

start(CoordinatorPID, ReceivingSocket) ->
  gen_server:start(?MODULE, [CoordinatorPID, ReceivingSocket], []).

init([CoordinatorPID, ReceivingSocket]) ->
  {ok, #state{coordinator_pid  = CoordinatorPID, receiving_socket = ReceivingSocket}}.

handle_cast(kill, State) ->
  utility:log("receiver: received kill message"),
  {stop, normal, State}; % calls :terminate and then shuts down

handle_cast(UnknownMessage, State) ->
  utility:log(io:format("received unknown msg: ~p~n", [UnknownMessage])),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, State) ->
  utility:log("receiver: closed receiving socket"),
  gen_udp:close(State#state.receiving_socket),
  utility:log("receiver: terminated"),
  ok.

handle_info({udp, _Socket, _IPtuple, _InPortNo, Packet}, State) ->
  utility:log("receiver: received packet"),
  Timestamp = utility:current_timestamp(),
  Slot      = utility:slot_of_timestamp(Timestamp),
  gen_server:cast(State#state.coordinator_pid, {received, Slot, Timestamp, Packet}),
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
