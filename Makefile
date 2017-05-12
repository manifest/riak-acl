PROJECT = riakacl
PROJECT_DESCRIPTION = ACL on top of Riak KV

DEP_PLUGINS = \
	version.mk

DEPS = \
	riakc

NO_AUTOPATCH = \
	riak_pb

dep_riakc = git https://github.com/basho/riak-erlang-client.git 2.5.3

TEST_DEPS = ct_helper
dep_ct_helper = git git://github.com/ninenines/ct_helper.git master

BUILD_DEPS = version.mk
dep_version.mk = git git://github.com/manifest/version.mk.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk

app:: rebar.config

export DEVELOP_ENVIRONMENT = $(shell if [ -f .develop-environment ]; then cat .develop-environment; fi)
