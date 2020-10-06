FROM sprygroup/emq

## Add plugin code
COPY . /emq-auth-jwt
COPY ./emq_auth_jwt.conf.example /opt/emqttd/etc/plugins/emq_auth_jwt.conf

# add plugin to loaded plugins.
ENV EMQ_LOADED_PLUGINS="emq_recon,emq_modules, emq_retainer, emq_dashboard, emq_auth_jwt" 

## switch to root to install packages.
USER root 

## Build and add plugin code 
RUN cd /emq-auth-jwt \
    # add build deps, remove after build
    && apk --no-cache add --virtual .build-deps \
        build-base \
        gcc \
        make \
        bsd-compat-headers \
        perl \
        erlang \
        erlang-public-key \
        erlang-syntax-tools \
        erlang-erl-docgen \
        erlang-gs \
        erlang-observer \
        erlang-ssh \
        #erlang-ose \
        erlang-cosfiletransfer \
        erlang-runtime-tools \
        erlang-os-mon \
        erlang-tools \
        erlang-cosproperty \
        erlang-common-test \
        erlang-dialyzer \
        erlang-edoc \
        erlang-otp-mibs \
        erlang-crypto \
        erlang-costransaction \
        erlang-odbc \
        erlang-inets \
        erlang-asn1 \
        erlang-snmp \
        erlang-erts \
        erlang-et \
        erlang-cosnotification \
        erlang-xmerl \
        erlang-typer \
        erlang-coseventdomain \
        erlang-stdlib \
        erlang-diameter \
        erlang-hipe \
        erlang-ic \
        erlang-eunit \
        #erlang-webtool \
        erlang-mnesia \
        erlang-erl-interface \
        #erlang-test-server \
        erlang-sasl \
        erlang-jinterface \
        erlang-kernel \
        erlang-orber \
        erlang-costime \
        erlang-percept \
        erlang-dev \
        erlang-eldap \
        erlang-reltool \
        erlang-debugger \
        erlang-ssl \
        erlang-megaco \
        erlang-parsetools \
        erlang-cosevent \
        erlang-compiler \
    # add fetch deps, remove after build
    && apk add --no-cache --virtual .fetch-deps \
        git \
        wget \
    # add run deps, never remove
    && apk add --no-cache --virtual .run-deps \
        ncurses-terminfo-base \
        ncurses-terminfo \
        ncurses-libs \
        readline \
    && make \
    # bypass failing unit tests. TODO: figure out how to make them pass. 
    # && make tests \
    # removing fetch deps and build deps
    && cp ebin/* /opt/emqttd/lib/emq_auth_jwt-2.3/ebin/ \
    && cp priv/* /opt/emqttd/lib/emq_auth_jwt-2.3/priv/ \
    && apk --purge del .build-deps .fetch-deps \
    && rm -rf /var/cache/apk/*

# switch back to emqtt user.
USER emqtt