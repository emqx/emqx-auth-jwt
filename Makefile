PROJECT = emq_auth_jwt
PROJECT_DESCRIPTION = Authentication with JWT
PROJECT_VERSION = 2.3.1

NO_AUTOPATCH = cuttlefish jwt

DEPS = jwt clique

dep_jwt    = git https://github.com/marianoguerra/jwt-erl
dep_clique = git https://github.com/emqtt/clique

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd develop
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_jwt.conf -i priv/emq_auth_jwt.schema -d data
