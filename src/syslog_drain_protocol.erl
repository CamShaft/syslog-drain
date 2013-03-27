-module(syslog_drain_protocol).

-include("syslog_drain.hrl").

-export([start_link/4, init/4]).

%% @doc Start an syslog protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

%% Internal.

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
  Parsers = syslog_drain:get_value(parsers, Opts, []),
  Emitters = syslog_drain:get_value(emitters, Opts, []),
  ok = ranch:accept_ack(ListenerPid),
  recv(<<>>, Socket, Transport, #handlers{parsers=Parsers, emitters=Emitters}).

%% @private
-spec recv(binary(), inet:socket(), module(), #handlers{}) -> ok.
recv(Buffer, Socket, Transport, Options) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer2 = syslog_drain_handler:handle(<<Buffer/binary, Data/binary>>, Options),
      recv(Buffer2, Socket, Transport, Options);
    {error, _}->
      terminate(Socket, Transport)
  end.

%% @private
-spec terminate(inet:socket(), module()) -> ok.
terminate(Socket, Transport)->
  Transport:close(Socket),
  ok.
