-module(cache).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get/1, set/2, set/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("cache.hrl").
-define(SERVER, ?MODULE).

-record(cache, {key, val, ttl}).
-record(state, {timer}).

-spec start_link() -> {ok, pid()}.
start_link() ->
	ok = mnesia:start(), % ok even if already started
	case mnesia:create_table(?CACHE_TABLE, [{attributes, record_info(fields, cache)}]) of
		{atomic,ok} ->
			true;
		{aborted,{already_exists,Info}} ->
			logger:notice("mnesia:create_table: already_exists: ~p", [Info]),
			true
	end,
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Expire time stored with record to check and cleanup on get().
%% Also scheduled separated periodic cleanup().

get(Key) ->
	Read = fun(Rkey) -> mnesia:read(?CACHE_TABLE, Rkey, read) end,
	Delete = fun(Dkey) -> mnesia:delete(?CACHE_TABLE, Dkey, write) end,
	{atomic, Result} = mnesia:transaction(Read, [Key]),
	case Result of
		[{_Tablename, Key, Val, infinity}] ->
			{ok, Val};
		[{_Tablename, Key, Val, Expire}] ->
			Now = erlang:system_time(second),
			if
				Expire < Now ->
					{atomic, ok} = mnesia:transaction(Delete, [Key]),
					{error, not_found};
				true ->
					{ok, Val}
			end;
		[] ->
			{error, not_found}
	end.

set(Key, Val) ->
	set(Key, Val, [{ttl, ?DEFAULT_TTL}]).

set(Key, Val, Opts) ->
	Insert = fun(Record) -> mnesia:write(?CACHE_TABLE, Record, write) end,
	case lists:keyfind(ttl, 1, Opts) of
		{ttl, 0} ->
			{atomic, ok} = mnesia:transaction(Insert, [#cache{key=Key, val=Val, ttl=infinity}]),
			ok;
		{ttl, infinity} ->
			{atomic, ok} = mnesia:transaction(Insert, [#cache{key=Key, val=Val, ttl=infinity}]),
			ok;
		{ttl, Ttl} ->
			Expire = erlang:system_time(second) + Ttl,
			{atomic, ok} = mnesia:transaction(Insert, [#cache{key=Key, val=Val, ttl=Expire}]),
			ok;
		false ->
			set(Key, Val)
	end.

cleanup() ->
	%% Delete outdated but not yet requested records.
	Cleanup_iterator = fun (#cache{key=Key}, _) ->
			case cache:get(Key) of
				{error, not_found} -> logger:notice("Cleanup: ~p", [Key]);
				_ -> true
			end
	end,
	Run = fun() ->	mnesia:foldr(Cleanup_iterator, [], ?CACHE_TABLE) end,
	mnesia:transaction(Run),

	{ok, New_timer} = timer:apply_after(timer:seconds(?CLEANUP_INTERVAL), gen_server, call, [?SERVER, cleanup]),
	#state{timer = New_timer}.

%% gen_server.

init([]) ->
	logger:notice("Cache started, cleanup interval: ~p", [?CLEANUP_INTERVAL]),
	{ok, Timer} = timer:apply_after(timer:seconds(?CLEANUP_INTERVAL), gen_server, call, [?SERVER, cleanup]),
	{ok, #state{timer = Timer}}.

handle_call(Request, _From, State) ->
	% logger:notice("Cache call from ~p: ~p", [From, Request]),
	case Request of
		cleanup ->
			New_state = cleanup(),
			{reply, ok, New_state};
		_ ->
			{reply, ok, State}
	end.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	logger:notice("Cache terminated: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

