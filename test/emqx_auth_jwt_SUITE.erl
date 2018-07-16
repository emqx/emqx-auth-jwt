%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_jwt_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("emqx/include/emqx.hrl").

-define(APP, emqx_auth_jwt).

all() ->
    [{group, emqx_auth_jwt}].

groups() ->
    [
    {emqx_auth_jwt, [sequence],
     [check_auth]}
    ].

init_per_suite(Config) ->
    [start_apps(App) || App <- [emqx, emqx_auth_jwt]],
    Config.

end_per_suite(_Config) ->
    [application:stop(App) || App <- [emqx_auth_jwt, emqx]].

check_auth(_) ->
    Plain = #mqtt_client{client_id = <<"client1">>, username = <<"plain">>},
    Jwt = jwerl:sign([{client_id, <<"client1">>}, {username, <<"plain">>}, {exp, os:system_time(seconds) + 10}], hs256, <<"emqsecret">>),
    ok = emqx_access_control:auth(Plain, Jwt),
    Jwt_Error = jwerl:sign([{client_id, <<"client1">>}, {username, <<"plain">>}], hs256,<<"secret">>),
    {error, token_error} = emqx_access_control:auth(Plain, Jwt_Error),
    Result =
    case emqx:env(allow_anonymous, false) of
        true  -> ok;
        false -> {error, "No auth module to check!"}
    end,
    ?assertEqual(Result, emqx_access_control:auth(Plain, <<"asd">>)).

start_apps(App) ->
    NewConfig = generate_config(App),
    lists:foreach(fun set_app_env/1, NewConfig).

generate_config(emqx) ->
    Schema = cuttlefish_schema:files([local_path(["deps", "emqx", "priv", "emqx.schema"])]),
    Conf = conf_parse:file([local_path(["deps", "emqx", "etc", "emqx.conf"])]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(?APP) ->
    Schema = cuttlefish_schema:files([local_path(["priv", "emqx_auth_jwt.schema"])]),
    Conf = conf_parse:file([local_path(["etc", "emqx_auth_jwt.conf"])]),
    cuttlefish_generator:map(Schema, Conf).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

get_base_dir() ->
    get_base_dir(?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).

set_app_env({App, Lists}) ->
    F = fun ({acl_file, _Var}) ->
                application:set_env(App, acl_file, local_path(["deps", "emqx", "etc", "acl.conf"]));
            ({license_file, _Var}) ->
                application:set_env(App, license_file, local_path(["deps", "emqx", "etc", "emqx.lic"]));
            ({plugins_loaded_file, _Var}) ->
                application:set_env(App, plugins_loaded_file, local_path(["deps","emqx","test", "emqx_SUITE_data","loaded_plugins"]));
            ({Par, Var}) ->
                application:set_env(App, Par, Var)
        end,
    lists:foreach(F, Lists),
    application:ensure_all_started(App).
