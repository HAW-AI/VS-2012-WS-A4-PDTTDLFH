-module(datasink).

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

-record(state, {team_number,
                station_number
				}).

start(TeamNumber, StationNumber) ->
  gen_server:start(?MODULE, [TeamNumber, StationNumber], []).

init([TeamNumber, StationNumber]) ->
  %%% start reading for the input source
  utility:log("datasink init"),
  {ok, #state{ team_number = TeamNumber,
               station_number = StationNumber
             }}.

handle_cast({write_data, StationIdentifier, StationNumber, Slot, _Payload, Timestamp, TimestampReceived}, State) ->
  utility:log(lists:concat(["team",State#state.team_number,"-station",State#state.station_number]),"~s~p, TX: ~p, next slot: ~p, at ~p~n",[StationIdentifier, StationNumber, Timestamp, Slot, TimestampReceived]),
  {noreply, State};
handle_cast(Any, State) ->
  utility:log(io_lib:format("received unknown msg: ~p~n",[Any])),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, _State) ->
  ok.

%%% OTP gen_server boilerplate - ignore this
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_info(_Info, State) ->
  utility:log(io_lib:format("how about sending gen server a msg in a proper way")),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.