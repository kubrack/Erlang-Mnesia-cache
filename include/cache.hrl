-hrl_name(cache).

-define(CACHE_TABLE, list_to_atom(os:getenv("CACHE_TABLE", "cache"))).
-define(DEFAULT_TTL, list_to_integer(os:getenv("DEFAULT_TTL", "60"))).
-define(CLEANUP_INTERVAL, list_to_integer(os:getenv("CLEANUP_INTERVAL", "3600"))).

