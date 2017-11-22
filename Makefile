PROJECT = emqx_auth_jwt
PROJECT_DESCRIPTION = Authentication with JWT
PROJECT_VERSION = 2.3.0

NO_AUTOPATCH = cuttlefish jwt

DEPS = jwt clique

dep_jwt    = git https://github.com/marianoguerra/jwt-erl
dep_clique = git https://github.com/emqtt/clique

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx-enterprise
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_jwt.conf -i priv/emqx_auth_jwt.schema -d data
