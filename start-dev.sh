#!/bin/sh
rebar compile && exec erl  -pa ebin deps/*/ebin  -eval "application:start(erlide)" -config priv/app.config