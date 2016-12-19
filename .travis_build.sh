#!/bin/bash

wget https://s3.amazonaws.com/rebar3/rebar3

chmod a+x ./rebar3
./rebar3 compile
./rebar3 as test dialyzer
./rebar3 eunit

ls -rt $(find ./logs -name "cover.html") | tail -n 1 | xargs cat | grep -F 'Total' | awk '{gsub("<[^>]*>", "")}1'