-module(heroku_metrics).

%% API.
-export([start/0]).

%% API.

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(syslog_drain),
  ok = application:start(heroku_metrics).
