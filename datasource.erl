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

-record(state, {}).

start() ->
  gen_server:start(?MODULE, [], []).

init([]) ->
  %%% start reading for the input source
  spawn(fun() -> read_from_datasource(self()) end),

  {ok, #state{}}.

handle_cast({input, Data}, State) ->
  %%% TODO handle input in here somehow
  {noreply, State};

handle_cast(kill, State) ->
  {stop, normal, State}.

%%% do everything required for a clean shutdown
terminate(_Reason, _State) ->
  ok.





read_from_datasource(DatasourcePID) ->
  case io:get_chars("", 24) of
    %%% TODO log something
    eof -> exit(normal);
    {error, Reason} -> exit(Reason);
    NextChunk ->
      gen_server:cast(DatasourcePID, {input, NextChunk}),
      read_from_datasource(DatasourcePID)
  end.


%%% OTP gen_server boilerplate - ignore this
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
