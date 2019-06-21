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

-export([ check/2
        , description/0
        ]).

%%------------------------------------------------------------------------------
%% Authentication callbacks
%%------------------------------------------------------------------------------

check(Credentials, Env = #{from := From, checklists := Checklists}) ->
    case maps:find(From, Credentials) of
        error -> {ok, Credentials#{auth_result => token_undefined, anonymous => false}};
        {ok, Token} ->
            try jwerl:header(Token) of
                Headers ->
                    case verify_token(Headers, Token, Env) of
                        {ok, Claims} ->
                            verify_claims(Checklists, Claims, Credentials);
                        {error, Reason} -> {stop, Credentials#{auth_result => Reason, anonymous => false}}
                    end
            catch
                _Error:Reason ->
                    ?LOG(error, "[JWT] Check token error: ~p", [Reason]), ok
            end
    end.

description() -> "Authentication with JWT".

%%------------------------------------------------------------------------------
%% Verify Token
%%------------------------------------------------------------------------------

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
    ?LOG(error, "[JWT] Unsupported token algorithm: ~p", [Header]),
    {error, token_unsupported}.

verify_token2(Alg, Token, SecretOrKey) ->
    try jwerl:verify(Token, decode_algo(Alg), SecretOrKey) of
        {ok, Claims}  ->
            {ok, Claims};
        {error, Reason} ->
            {error, Reason}
    catch
        _Error:Reason ->
            {error, Reason}
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

%%------------------------------------------------------------------------------
%% Verify Claims
%%------------------------------------------------------------------------------

verify_claims(Checklists, Claims, Credentials) ->
    case do_verify_claims(feedvar(Checklists, Credentials), Claims) of
        {error, Reason} ->
            {stop, Credentials#{auth_result => {error, Reason}, anonymous => false}};
        ok ->
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

