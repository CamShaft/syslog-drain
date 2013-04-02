#!/bin/sh
erl -pa ebin deps/*/ebin -s heroku_metrics \
  -eval "io:format(\"Syslog drain listening on localhost:10514~n\")."
