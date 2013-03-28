-module (syslog_drain_stdout_emitter).

-include_lib("eunit/include/eunit.hrl").

-export([emit/1]).

emit(Messages)->
  io:format("~n~p~n", [Messages]).
