#!/bin/ash

# This script should run end user integration tests and return non-zero on errors to fail the docker build.

# give it a little pause to allow containers to start.
sleep 10

# execute tests... write appropriate external integration tests.
mosquitto_pub -t 'pub' -m 'hello' -h  emqttd -i test -u test -P eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiYm9iIiwiYWdlIjoyOX0.bIV_ZQ8D5nQi0LT8AVkpM4Pd6wmlbpR9S8nOLJAsA8o
