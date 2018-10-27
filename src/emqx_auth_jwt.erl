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

-module(emqx_auth_jwt).

-include_lib("emqx/include/emqx.hrl").

-behaviour(emqx_auth_mod).

%% emqx_auth_mod callbacks
-export([init/1, check/3, description/0]).

%%--------------------------------------------------------------------
%% emqx_auth_mod Callbacks
%%--------------------------------------------------------------------

init(Env) ->
    {ok, Env}.

check(_Credentials, undefined, _Env) ->
    {error, token_undefined};
check(_Credentials, Token, Env) ->
    case catch jwerl:header(Token) of
        {'EXIT', _} -> ignore; % Not a JWT Token
        Headers ->
            verify_token(Headers, Token, Env)
    end.

verify_token(#{alg := <<"HS", _/binary>>}, _Token, #{secret := undefined}) ->
    {error, hmac_secret_undefined};
verify_token(#{alg := Alg = <<"HS", _/binary>>}, Token, #{secret := Secret}) ->
    verify_token2(Alg, Token, Secret);
verify_token(#{alg := <<"RS", _/binary>>}, _Token, #{pubkey := undefined}) ->
    {error, rsa_pubkey_undefined};
verify_token(#{alg := Alg = <<"RS", _/binary>>}, Token, #{pubkey := PubKey}) ->
    verify_token2(Alg, Token, PubKey);
verify_token(#{alg := <<"ES", _/binary>>}, _Token, #{pubkey := undefined}) ->
    {error, ecdsa_pubkey_undefined};
verify_token(#{alg := Alg = <<"ES", _/binary>>}, Token, #{pubkey := PubKey}) ->
    verify_token2(Alg, Token, PubKey);
verify_token(Header, _Token, _Env) ->
    logger:error("Unsupported token: ~p", [Header]),
    {error, token_unsupported}.

verify_token2(Alg, Token, SecretOrKey) ->
    case catch jwerl:verify(Token, decode_algo(Alg), SecretOrKey) of
        {ok, _Claims}  ->
            ok;
        {error, Reason} ->
            logger:error("JWT decode error:~p", [Reason]),
            {error, token_error};
        {'EXIT', Error} ->
            logger:error("JWT decode error:~p", [Error]),
            {error, token_error}
    end.

decode_algo(<<"HS256">>) -> hs256;
decode_algo(<<"HS384">>) -> hs384;
decode_algo(<<"HS512">>) -> hs512;
decode_algo(<<"RS256">>) -> rs256;
decode_algo(<<"RS384">>) -> rs384;
decode_algo(<<"RS512">>) -> rs512;
decode_algo(<<"ES256">>) -> es256;
decode_algo(<<"ES384">>) -> es384;
decode_algo(<<"ES512">>) -> es512;
decode_algo(<<"none">>)  -> none;
decode_algo(Alg) -> throw({error, {unsupported_algorithm, Alg}}).

description() -> "Authentication with JWT".
