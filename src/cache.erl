-module(cache).

-include("cache.hrl").

-record(cache, {key, val}).

%% API.
-export([get/1, set/2, set/3, start/0]).

%% API implementation.

get(Key) ->
	case mnesia:dirty_read(?CACHE_TABLE, Key) of
		[{_Tablename, Key, Val}] ->
			{ok, Val};
		[] ->
			{error, not_found}
	end.

set(Key, Val) ->
	set(Key, Val, [{ttl, ?DEFAULT_TTL}]).

set(Key, Val, Opts) ->
	case lists:keyfind(ttl, 1, Opts) of
		{ttl, 0} ->
			ok;
		{ttl, infinity} ->
			mnesia:dirty_write(?CACHE_TABLE, #cache{key=Key, val=Val}); % ok 
		{ttl, Ttl} -> 
			timer:apply_after(timer:seconds(Ttl), mnesia, dirty_delete, [?CACHE_TABLE, Key]),
			mnesia:dirty_write(?CACHE_TABLE, #cache{key=Key, val=Val}); % ok 
		false ->
			set(Key, Val)
	end.

start() ->
	ok = mnesia:start(),
	case mnesia:create_table(?CACHE_TABLE, [{attributes, record_info(fields, cache)}]) of
		{atomic,ok} -> 
			true;
		{aborted,{already_exists,Info}} -> 
			logger:notice("mnesia:create_table: already_exists: ~p", [Info]),
			true
	end.

