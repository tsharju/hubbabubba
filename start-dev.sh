#!/bin/bash

erl \
    -pa apps/hubbabubba/ebin/ \
    -pa deps/ibrowse/ebin/ \
    -pa deps/mochiweb/ebin/ \
    -pa deps/webmachine/ebin/ \
    -boot start_sasl \
    -s reloader \
    -s hubbabubba \
    -sname hubbabubba
