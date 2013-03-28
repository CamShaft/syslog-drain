-module(heroku_metrics_stdout_emitter).

-export([emit/1]).

emit(Messages)->
  io:format("~p~n", [Messages]).
