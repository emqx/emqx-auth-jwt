%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emqx_auth_jwt).

-include_lib("emqx/include/emqx.hrl").

-include_lib("jwt/include/jwt.hrl").

-behaviour(emqx_auth_mod).

%% emqx_auth_mod callbacks
-export([init/1, check/3, description/0]).

%%--------------------------------------------------------------------
%% emqx_auth_mod Callbacks
%%--------------------------------------------------------------------

init(Env) ->
    {ok, Env}.

check(_Client, undefined, _Env) ->
    {error, token_undefined};
check(_Client, Token, Env) ->
    verify_token(jwerl:header(Token), Token, Env).

verify_token(#{alg := <<"HS", _/binary>>}, _Token, #{secret := undefined}) ->
    {error, hmac_secret_undefined};
verify_token(#{alg := <<"HS", _/binary>>}, Token, #{secret := Secret}) ->
    verify_token(Token, Secret);
verify_token(#{alg := <<"RS", _/binary>>}, _Token, #{pubkey := undefined}) ->
    {error, rsa_pubkey_undefined};
verify_token(#{alg := <<"RS", _/binary>>}, Token, #{pubkey := PubKey}) ->
    verify_token(Token, PubKey);
verify_token(#{alg := <<"ES", _/binary>>}, _Token, #{pubkey := undefined}) ->
    {error, ecdsa_pubkey_undefined};
verify_token(#{alg := <<"ES", _/binary>>}, Token, #{pubkey := PubKey}) ->
    verify_token(Token, PubKey);
verify_token(Header, _Token, _Env) ->
    lager:error("Unsupported token: ~p", [Header]),
    {error, token_unsupported}.

verify_token(Token, SecretOrKey) ->
    case catch jwerl:verify(Token, SecretOrKey, false) of
        {ok, _Jwt}      -> ok;
        {error, Reason} ->
            lager:error("JWT decode error:~p", [Reason]),
            {error, token_error};
        {'EXIT', Error} ->
            lager:error("JWT decode error:~p", [Error]),
            {error, token_error}
    end.

description() ->
    "Authentication with JWT".

