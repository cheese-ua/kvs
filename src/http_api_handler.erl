-module(http_api_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").

init({tcp, http}, Req, Opts) ->

  {ok, Req, undefined_state}.

handle(Req, State) ->
  {Url, _} = cowboy_req:url(Req),
  {HostUrl, _} = cowboy_req:host_url(Req),
  ?INFO("Url: ~p~n", [Url]),
  ?INFO("HostUrl: ~p~n", [HostUrl]),
  Params = lists:subtract(binary_to_list(Url), binary_to_list(HostUrl)),
  ?INFO("Params: ~p~n", [Params]),
  [Action | _Tokens] = string:tokens(Params, "/"),
  {Key, Req2} = cowboy_req:qs_val(<<"key">>, Req, <<>>),
  {Val, Req3} = cowboy_req:qs_val(<<"val">>, Req2, <<>>),
  ?INFO("Key: ~p Val: ~p~n", [Key, Val]),
  Reply1 = io_lib:format("action: ~p, key: ~p, val: ~p", [Action, Key, Val]),
  Res = process_query(Action, Key, Val),
  Reply2 = io_lib:format("~p", [Res]),
  {ok, Req4} = cowboy_req:reply(200, [], list_to_binary([Reply1, "<br/>", Reply2 ]), Req3),
  {ok, Req4, State}.


terminate(Reason, Req, State) ->
  ok.

process_query("create", Key, Value) ->
  kvs:create(Key, Value);
process_query("read", Key, _Value) ->
  kvs:read(Key);
process_query("delete", Key, _Value) ->
  kvs:delete(Key);
process_query("update", Key, Value) ->
  kvs:update(Key, Value);
process_query(Action, Key, Value) ->
  {unknown_action, Action, Key, Value}.

