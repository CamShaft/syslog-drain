-module(heroku_metrics_stdout_emitter).

-export([send/1]).

send(Messages)->
  io:format("~p~n", [Messages]).
