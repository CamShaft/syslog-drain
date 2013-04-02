%%
%% syslog_drain.erl
%% syslog_drain entry point
%%
-module (syslog_drain).

-export([start_server/4]).
-export([stop_listener/1]).
-export([start_link/4]).
-export([init/3]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, ProtoOpts) when is_integer(NbAcceptors), NbAcceptors > 0 ->
  ranch:start_listener(Ref, NbAcceptors, ranch_tcp, TransOpts, ?MODULE, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).

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
