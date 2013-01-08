-module(datasource).

-behaviour(gen_server).

%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {input_reader_pid :: pid(), current_data=[]}).

start() ->
  gen_server:start(?MODULE, [], []).

init([]) ->
  %%% start reading for the input source
  utility:log("datasource init"),
  DatasourcePID = self(),
  InputReaderPID = spawn(fun() -> read_from_datasource(DatasourcePID) end),

  {ok, #state{input_reader_pid = InputReaderPID}}.

handle_cast({input, Data}, State) ->
  utility:log("new data stored: ~p~n",[Data]),
  {noreply, State#state{current_data=Data}};

handle_cast({get_data, PID}, State) ->
  utility:log(io_lib:format("data transfered to sender: ~p~n",[State#state.current_data])),
  gen_fsm:send_event(PID,{input, State#state.current_data}),
  {noreply, State#state{current_data=[]}};

handle_cast(kill, State) ->
  utility:log("datasource killed"),
  {stop, normal, State};

handle_cast(Any, State) ->
  utility:log(io_lib:format("received unknown msg: ~p~n",[Any])),
  {noreply, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, State) ->
  exit(State#state.input_reader_pid, normal),
  ok.

read_from_datasource(DatasourcePID) ->
  case io:get_chars("", 24) of
    eof ->
      exit(normal);
    {error, Reason} ->
      exit(Reason);
    NextChunk ->
      gen_server:cast(DatasourcePID, {input, NextChunk}),
      read_from_datasource(DatasourcePID)
  end.


%%% OTP gen_server boilerplate - ignore this
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_info(_Info, State) ->
  utility:log(io_lib:format("how about sending gen server a msg in a proper way")),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
