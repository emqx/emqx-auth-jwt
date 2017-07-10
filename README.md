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

Algorithms
----------

The JWT spec supports several algorithms for cryptographic signing. This library
currently supports:

* HS256 - HMAC using SHA-256 hash algorithm
* HS384 - HMAC using SHA-384 hash algorithm
* HS512 - HMAC using SHA-512 hash algorithm


License
-------

Apache License Version 2.0

Author
------

Feng at emqtt.io
