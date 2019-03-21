%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_jwt_app).

-behaviour(application).
-behaviour(supervisor).

-emqx_plugin(?MODULE).

-export([start/2, stop/1]).
-export([init/1]).

-define(APP, emqx_auth_jwt).

-define(JWT_ACTION, {emqx_auth_jwt, check, [auth_env()]}).

start(_Type, _Args) ->
    emqx:hook('client.authenticate', ?JWT_ACTION),
    emqx_auth_jwt_cfg:register(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    emqx:unhook('client.authenticate', ?JWT_ACTION),
    emqx_auth_jwt_cfg:unregister().

%%--------------------------------------------------------------------
%% Dummy supervisor
%%--------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_all, 1, 10}, []} }.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

auth_env() ->
    #{secret => application:get_env(?APP, secret, undefined),
      from => application:get_env(?APP, from, password),
      pubkey => read_pubkey()}.

read_pubkey() ->
    case application:get_env(?APP, pubkey) of
        undefined  -> undefined;
        {ok, Path} -> {ok, PubKey} = file:read_file(Path),
                      PubKey
    end.

