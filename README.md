# emq-auth-jwt
EMQ JWT Authentication Plugin

Build
-----

```
make && make tests
```

Configure the Plugin
--------------------

File: etc/emq_auth_jwt.conf

```
auth.jwt.secret = emqsecret
```


Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_auth_jwt
```

Example
-------

```
mosquitto_pub -t 'pub' -m 'hello' -i test -u test -P eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiYm9iIiwiYWdlIjoyOX0.bIV_ZQ8D5nQi0LT8AVkpM4Pd6wmlbpR9S8nOLJAsA8o
```
Algorithms
----------

The JWT spec supports several algorithms for cryptographic signing. This library
currently supports:

* HS256 - HMAC using SHA-256 hash algorithm
* HS384 - HMAC using SHA-384 hash algorithm
* HS512 - HMAC using SHA-512 hash algorithm

* RS256 - RSA with the SHA-256 hash algorithm
* RS384
* RS512

* ES256 - ECDSA using the P-256 curve
* ES384
* ES512

License
-------

Apache License Version 2.0

Author
------

Feng at emqtt.io
