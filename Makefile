PROJECT = emq_mod_subscription
PROJECT_DESCRIPTION = Subscription Management Module
PROJECT_VERSION = 2.0.7

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq20
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_mod_subscription.conf -i priv/emq_mod_subscription.schema -d data
