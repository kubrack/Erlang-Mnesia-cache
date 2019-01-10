PROJECT = cache
PROJECT_DESCRIPTION = DevelopEx test task
PROJECT_VERSION = 0.1.0

include erlang.mk

func-test: all
	erl -env DEFAULT_TTL 3 -noinput -pa ebin/ -s cache_test


