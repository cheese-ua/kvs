-module(kvs).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("logger.hrl").

-type(key() :: binary() | string() | atom()).
-type(value() :: term()).
-type(value_time() :: {calendar:datetime(), value()}).
-type(key_set() :: {key(), [value_time()]}).

-record(state, {
  items = [] :: [key_set()]
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, create/2, read/1, update/2, delete/1, clear/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec(create(key(), value()) -> ok).
create(Key, Val) ->
  gen_server:call(?SERVER, {create, Key, Val}),
  ok.

-spec(read(key()) -> key_set() | {error, not_found}).
read(Key) ->
  gen_server:call(?SERVER, {read, Key}).

-spec(update(key(), value()) -> ok | {error, not_found}).
update(Key, Val) ->
  gen_server:call(?SERVER, {update, Key, Val}).

-spec(delete(key()) -> ok | {error, not_found}).
delete(Key) ->
  gen_server:call(?SERVER, {delete, Key}).

-spec(clear() -> ok).
clear() ->
  gen_server:call(?SERVER, clear),
  ok.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Options) ->
  ?INFO("KVS options: ~p~n", [Options]),
  FileName = proplists:get_value(data_file, Options),
  FlushInterval = proplists:get_value(flush_interval, Options, 20),
  self() ! {do_heavy_init, FileName},
  timer:send_interval(FlushInterval* 1000, self(), {flush, FileName}),
  {ok, #state{}}.

handle_call(clear, _From, State) ->
  {reply, ok, State#state{items = []}};

handle_call({delete, Key}, _From, #state{items = Items} = State) ->
  case proplists:get_value(Key, Items) of
    undefined ->
      {reply, {error, not_found}, State};
    _Values ->
      NewItems = proplists:delete(Key, Items),
      {reply, ok, State#state{items = NewItems}}
  end;

handle_call({update, Key, Value}, _From, #state{items = Items} = State) ->
  case proplists:get_value(Key, Items) of
    undefined ->
      {reply, {error, not_found}, State};
    [_Head | Tail] ->
      DT = {date(), time()},
      NewVal = [{DT, Value} | Tail],
      KeySet = {Key, NewVal},
      {reply, ok, update_state(KeySet, Key, State)}
  end;

handle_call({read, Key}, _From, #state{items = Items} = State) ->
  %%io:format("read key ~p when items ~p~n", [Key, Items]),
  case proplists:get_value(Key, Items) of
    undefined ->
      {reply, {error, not_found}, State};
    Values ->
      {reply, {Key, Values}, State}
  end;

handle_call({create, Key, Val}, _From, #state{items = Items} = State) ->
    DT = {date(), time()},
    case proplists:get_value(Key, Items) of
      undefined ->
        KeySet = {Key, [{DT, Val}]},
        NewItems = [KeySet | Items],
        %%io:format("after create key ~p items ~p~n", [Key, NewItems]),
        NewState =  State#state{items = NewItems},
        {reply, ok, NewState};
      Values ->
        NewVal = add_value({DT, Val} , Values),
        KeySet = {Key, NewVal},
        {reply, ok, update_state(KeySet, Key, State)}
    end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({flush, FileName}, #state{items=Items} = State) ->
  save_to_file(FileName, Items),
  {noreply, State};
handle_info({do_heavy_init, FileName}, State) ->
  Items = read_from_file(FileName),
  ?INFO("kvs inited with items: ~p~n", [Items]),
  {noreply, State#state{items = Items}};
handle_info(show_state, State) ->
  ?INFO("State: ~p~n", [State]),
  {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
add_value(NewValueTime, [Old1, Old2 | _]) ->
  [NewValueTime, Old1, Old2];
add_value(NewValueTime, Values) ->
   [NewValueTime | Values].

update_state(NewKeySet, OldKey, #state{items = Items} = State) ->
    Items2 =  proplists:delete(OldKey, Items),
    State#state{items = [NewKeySet | Items2]}.

-spec(save_to_file(string(), [key_set()]) -> ok).
save_to_file(FileName, Items) ->
  Bin = term_to_binary(Items),
  file:write_file(FileName, Bin),
  ?INFO("Save to file items: ~p~n", [Items]),
  ok.

-spec(read_from_file(string()) -> [key_set()]).
read_from_file(undefined) ->
  throw ("file not found");
read_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Data} ->
      binary_to_term(Data);
  _ -> []
  end.
