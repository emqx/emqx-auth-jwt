.PHONY: tests

PROJECT = emqx_auth_jwt
PROJECT_DESCRIPTION = EMQ X Authentication with JWT
PROJECT_VERSION = 3.0

NO_AUTOPATCH = cuttlefish jwerl

DEPS = jwerl clique

dep_jwerl  = git https://github.com/G-Corp/jwerl 1.0.0
dep_clique = git https://github.com/emqx/clique

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqtt/cuttlefish emqx30

NO_AUTOPATCH = cuttlefish jwerl

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_jwt.conf -i priv/emqx_auth_jwt.schema -d data
