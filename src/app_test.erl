-module(app_test).

-include("logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0]).

all() ->
  {ok, Mode} = application:get_env(kvs, mode),
  ?INFO("Mode: ~p~n", [Mode]),
  case Mode of
    test -> ok;
    _ -> throw ("can't run test in production")
  end,

  kvs_create_read_test(),
  kvs_update_test(),
  kvs_delete_test(),
  ok.

kvs_create_read_test() ->
  kvs:clear(),
  kvs:create(key1, 22),
  ?assertMatch({key1, [{_, 22}]}, kvs:read(key1)),
  kvs:create(key1, 23),
  ?assertMatch({key1, [{_, 23}, {_, 22}]}, kvs:read(key1)),
  kvs:create(key1, 24),
  ?assertMatch({key1, [{_, 24},{_, 23}, {_, 22}]}, kvs:read(key1)),
  kvs:create(key1, 25),
  ?assertMatch({key1, [{_, 25},{_, 24}, {_, 23}]}, kvs:read(key1)),

  ?assertEqual({error, not_found}, kvs:read(key2)),

  kvs:create(key2, 22),
  ?assertMatch({key2, [{_, 22}]}, kvs:read(key2)),
  kvs:create(key2, 23),
  ?assertMatch({key2, [{_, 23}, {_, 22}]}, kvs:read(key2)),
  kvs:create(key2, 24),
  ?assertMatch({key2, [{_, 24},{_, 23}, {_, 22}]}, kvs:read(key2)),
  kvs:create(key2, 25),
  ?assertMatch({key2, [{_, 25},{_, 24}, {_, 23}]}, kvs:read(key2)),

  ?assertMatch({key1, [{_, 25},{_, 24}, {_, 23}]}, kvs:read(key1)),
  kvs:clear(),
  ok.

kvs_update_test() ->
  kvs:clear(),
  kvs:create(key1, 22),
  ?assertMatch({key1, [{_, 22}]}, kvs:read(key1)),
  kvs:update(key1, 23),
  ?assertMatch({key1, [{_, 23}]}, kvs:read(key1)),
  kvs:update(key1, 22),
  ?assertMatch({key1, [{_, 22}]}, kvs:read(key1)),
  kvs:create(key1, 23),
  kvs:update(key1, 22),
  ?assertMatch({key1, [{_, 22},{_, 22}]}, kvs:read(key1)),

  kvs:create(key2, 22),
  ?assertMatch({key2, [{_, 22}]}, kvs:read(key2)),
  kvs:update(key2, 23),
  ?assertMatch({key2, [{_, 23}]}, kvs:read(key2)),
  kvs:update(key2, 22),
  ?assertMatch({key2, [{_, 22}]}, kvs:read(key2)),
  kvs:create(key2, 23),
  kvs:update(key2, 22),
  ?assertMatch({key2, [{_, 22},{_, 22}]}, kvs:read(key2)),

  kvs:update(key1, 23),
  ?assertMatch({key1, [{_, 23},{_, 22}]}, kvs:read(key1)),

  kvs:clear(),
  ok.


kvs_delete_test() ->
  kvs:clear(),

  ?assertEqual({error, not_found}, kvs:delete(key1)),

  kvs:create(key1, 22),
  ?assertEqual(ok, kvs:delete(key1)),
  ?assertEqual({error, not_found}, kvs:read(key1)),

  kvs:create(key2, 22),
  kvs:create(key1, 22),
  ?assertEqual(ok, kvs:delete(key1)),
  ?assertEqual({error, not_found}, kvs:read(key1)),
  ?assertMatch({key2, [{_, 22}]}, kvs:read(key2)),
  ok.