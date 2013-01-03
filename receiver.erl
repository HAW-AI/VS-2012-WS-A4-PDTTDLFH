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
  gen_udp:controlling_process(ReceivingSocket, self()),

  {ok, #state{coordinator_pid  = CoordinatorPID,
              receiving_socket = ReceivingSocket}}.

handle_cast({udp, _Socket, _IP, _InPortNo, Packet}, State) ->
  Slot = 1234567890, % FIXME
  Time = foo,        % FIXME

  gen_server:cast(State#state.coordinator_pid,
                  {received, Slot, Time, Packet}),
  {noreply, State};

handle_cast(kill, State) ->
  gen_udp:close(State#state.receiving_socket),
  exit(normal),
  {noreply, State}.




%%% OTP gen_server boilerplate - ignore this
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
