
# emqx-auth-jwt

EMQ X JWT Authentication Plugin

Build
-----

```
make && make tests
```

Configure the Plugin
--------------------

File: etc/plugins/emqx_auth_jwt.conf

```
## HMAC Hash Secret.
##
## Value: String
auth.jwt.secret = emqsecret

## RSA or ECDSA public key file.
##
## Value: File
## auth.jwt.pubkey = etc/certs/jwt_public_key.pem
```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_auth_jwt
```

Example
-------

```
mosquitto_pub -t 'pub' -m 'hello' -i test -u test -P eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiYm9iIiwiYWdlIjoyOX0.bIV_ZQ8D5nQi0LT8AVkpM4Pd6wmlbpR9S8nOLJAsA8o
```

Algorithms
----------

The JWT spec supports several algorithms for cryptographic signing. This plugin currently supports:

* HS256 - HMAC using SHA-256 hash algorithm
* HS384 - HMAC using SHA-384 hash algorithm
* HS512 - HMAC using SHA-512 hash algorithm

* RS256 - RSA with the SHA-256 hash algorithm
* RS384 - RSA with the SHA-384 hash algorithm
* RS512 - RSA with the SHA-512 hash algorithm

* ES256 - ECDSA using the P-256 curve
* ES384 - ECDSA using the P-384 curve
* ES512 - ECDSA using the P-512 curve

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.
