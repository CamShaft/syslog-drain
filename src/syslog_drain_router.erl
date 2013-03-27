-module(syslog_drain_router).

-include("syslog_drain.hrl").

-export([compile/1]).
-export([match/3]).

compile(_)->
  {}.

match(_, _, _)->
  ok.
