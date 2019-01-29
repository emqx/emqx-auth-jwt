.PHONY: tests

PROJECT = emqx_auth_jwt
PROJECT_DESCRIPTION = EMQ X Authentication with JWT
PROJECT_VERSION = 3.1

NO_AUTOPATCH = cuttlefish

DEPS = jwerl clique

dep_jwerl  = git-emqx https://github.com/G-Corp/jwerl 1.0.0
dep_clique = git-emqx https://github.com/emqx/clique v0.3.11

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx testing
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1


ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_jwt.conf -i priv/emqx_auth_jwt.schema -d data
