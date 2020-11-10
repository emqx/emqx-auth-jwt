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

-module(emqx_auth_jwt).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-logger_header("[JWT]").

-export([ register_metrics/0
        , check/2
        , description/0
        ]).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['auth.jwt.success', 'auth.jwt.failure', 'auth.jwt.ignore']].

%%------------------------------------------------------------------------------
%% Authentication callbacks
%%------------------------------------------------------------------------------

check(Credentials, #{from := From, checklists := Checklists}) ->
    case maps:find(From, Credentials) of
        error ->
            ?LOG(error, "From ~p not defined in system~n", [From]),
            emqx_metrics:inc('auth.jwt.ignore'), ok;
        {ok, Token} ->
            case emqx_auth_jwt_svr:verify(Token) of
                {error, not_found} ->
                    ?LOG(info, "Not found suitable JWKs/Secret/Pubkey~n"),
                    emqx_metrics:inc('auth.jwt.ignore'), ok;
                {error, not_token} ->
                    ?LOG(info, "The token value is not a valid JWT format~n"),
                    emqx_metrics:inc('auth.jwt.ignore'), ok;
                {error, Reason} ->
                    ?LOG(warning, "Checking token failed: ~p~n", [Reason]),
                    emqx_metrics:inc('auth.jwt.failure'),
                    {stop, Credentials#{auth_result => Reason, anonymous => false}};
                {ok, Claims} ->
                    verify_claims(Checklists, Claims, Credentials)
            end
    end.

description() -> "Authentication with JWT".

%%------------------------------------------------------------------------------
%% Verify Claims
%%------------------------------------------------------------------------------

verify_claims(Checklists, Claims, Credentials) ->
    case do_verify_claims(feedvar(Checklists, Credentials), Claims) of
        {error, Reason} ->
            emqx_metrics:inc('auth.jwt.failure'),
            {stop, Credentials#{auth_result => {error, Reason}, anonymous => false}};
        ok ->
            emqx_metrics:inc('auth.jwt.success'),
            {stop, Credentials#{jwt_claims => Claims,
                                anonymous => false,
                                auth_result => success}}
    end.

do_verify_claims([], _Claims) ->
    ok;
do_verify_claims([{Key, Expected} | L], Claims) ->
    case maps:get(Key, Claims, undefined) =:= Expected of
        true -> do_verify_claims(L, Claims);
        false -> {error, list_to_atom("unexpected_" ++ atom_to_list(Key))}
    end.

feedvar(Checklists, #{username := Username, client_id := ClientId}) ->
    lists:map(fun({K, <<"%u">>}) -> {K, Username};
                 ({K, <<"%c">>}) -> {K, ClientId};
                 ({K, Expected}) -> {K, Expected}
              end, Checklists).
