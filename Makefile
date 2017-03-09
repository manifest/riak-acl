PROJECT = riakacl
PROJECT_DESCRIPTION = ACL on top of Riak KV
PROJECT_VERSION = 0.1.0

DEPS = \
	riakc

dep_riakc = git https://github.com/basho/riak-erlang-client.git 2.5.0

TEST_DEPS = ct_helper
dep_ct_helper = git git://github.com/ninenines/ct_helper.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk