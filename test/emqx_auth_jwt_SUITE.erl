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
    [{emqx_auth_jwt, [sequence], [check_auth]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:set_env(emqx_auth_jwt, secret, "emqxsecret"),
    Apps = [start_apps(App, DataDir) || App <- [emqx, emqx_auth_jwt]],
    ct:log("Apps: ~p~n", [Apps]),
    Config.

end_per_suite(_Config) ->
    [application:stop(App) || App <- [emqx_auth_jwt, emqx]].

check_auth(_) ->
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    Jwt = jwerl:sign([{client_id, <<"client1">>},
                      {username, <<"plain">>},
                      {exp, os:system_time(seconds) + 1}], hs256, <<"emqxsecret">>),
    ct:pal("Jwt: ~p~n", [Jwt]),
    ok = emqx_access_control:authenticate(Plain, Jwt),

    ct:sleep(1000),
    {error,token_error} = emqx_access_control:authenticate(Plain, Jwt),

    Jwt_Error = jwerl:sign([{client_id, <<"client1">>},
                            {username, <<"plain">>}], hs256, <<"secret">>),
    {error, token_error} = emqx_access_control:authenticate(Plain, Jwt_Error),
    ?assertEqual(case emqx_config:get_env(allow_anonymous, false) of
                     true  -> ok;
                     false -> {error, auth_modules_not_found}
                 end, emqx_access_control:authenticate(Plain, <<"asd">>)).

start_apps(App, _DataDir) ->
    application:ensure_all_started(App).
%start_apps(App, DataDir) ->
%    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
%    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
%    NewConfig = cuttlefish_generator:map(Schema, Conf),
%    Vals = proplists:get_value(App, NewConfig),
%    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
%    application:ensure_all_started(App).
