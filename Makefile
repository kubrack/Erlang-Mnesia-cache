PROJECT = cache
PROJECT_DESCRIPTION = Mnesia cache
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = mnesia

include erlang.mk

func-test: all
	erl -env CLEANUP_INTERVAL 2 -env DEFAULT_TTL 3 -noinput -pa ebin/ -s cache_test


