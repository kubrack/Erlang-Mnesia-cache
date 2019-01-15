-module(cache_test).
-include("cache.hrl").
-export([start/0, get_n_log/3]).

test_rec(Key, Val, Ttl) ->
	{error, not_found} = cache:get(Key),
	Dttl = ?DEFAULT_TTL,
	case Ttl of
		0 ->
			ok = cache:set(Key, Val, [{ttl, Ttl}]),
			get_n_log(Key, {error, not_found}, "zero");
		Dttl ->
			ok = cache:set(Key, Val),
			get_n_log(Key, {ok, Val}, integer_to_list(Ttl) ++ " default immediately"),
			ok = timer:sleep(timer:seconds(Ttl+1)),
			get_n_log(Key, {error, not_found}, integer_to_list(Ttl) ++ " default + 1");
		infinity ->
			ok = cache:set(Key, Val, [{ttl, Ttl}]),
			get_n_log(Key, {ok, Val}, "infinity immediately"),
			ok = timer:sleep(timer:seconds(1)),
			get_n_log(Key, {ok, Val}, "infinity + 1");
		_ ->
			ok = cache:set(Key, Val, [{ttl, Ttl}]),
			get_n_log(Key, {ok, Val}, integer_to_list(Ttl) ++ " immediately"),
			ok = timer:sleep(timer:seconds(Ttl+1)),
			get_n_log(Key, {error, not_found}, integer_to_list(Ttl) ++ " + 1")
	end.

get_n_log(Key, Res, Info) ->
	Res = cache:get(Key),
	io:format("ok ~p: ~p, ~p~n", [Info, Key, Res]).

start() ->
	application:ensure_all_started(cache),
	ok = cache:set(key6, val6, [{ttl, 1}]), % cleanup
	ok = cache:set(key7, val7, [{ttl, 1}]), % cleanup
	ok = cache:set(key8, val8, [{ttl, 1}]), % cleanup
	test_rec(key1, val1, 0),
	test_rec(key2, val2, ?DEFAULT_TTL),
	test_rec(key3, val3, infinity),
	test_rec(key4, val4, 1),
	test_rec(key5, val5, 2),
	erlang:halt().

