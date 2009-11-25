#!/bin/bash

erl \
    -pa ./ebin/ \
    -boot start_sasl \
    -s reloader \
    -s hubbabubba \
    -mnesia dir '"/tmp/Hubbabubba.db"' \
    -sname hubbabubba
