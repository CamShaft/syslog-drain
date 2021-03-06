-module(heroku_metrics).

%% API.
-export([start/0]).

%% API.

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(syslog_pipeline),
  ok = application:start(syslog_pipeline_tcp),
  ok = application:start(heroku_metrics).
