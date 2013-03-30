-module(syslog_drain_protocol).

-export([start_link/4, init/3]).

%% @doc Start an syslog protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, _Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport]),
  {ok, Pid}.

%% Internal.

-spec init(pid(), inet:socket(), module()) -> ok.
init(ListenerPid, Socket, Transport) ->
  ok = ranch:accept_ack(ListenerPid),
  recv(<<>>, Socket, Transport).

%% @private
-spec recv(binary(), inet:socket(), module()) -> ok.
recv(<<>>, Socket, Transport) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer = syslog_pipeline:handle(Data),
      recv(Buffer, Socket, Transport);
    {error, _}->
      terminate(Socket, Transport)
  end;
recv(Buffer, Socket, Transport) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer2 = syslog_pipeline:handle(<<Buffer/binary, Data/binary>>),
      recv(Buffer2, Socket, Transport);
    {error, _}->
      terminate(Socket, Transport)
  end.

%% @private
-spec terminate(inet:socket(), module()) -> ok.
terminate(Socket, Transport)->
  Transport:close(Socket),
  ok.
