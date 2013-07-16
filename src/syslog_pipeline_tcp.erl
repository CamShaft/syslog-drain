%%
%% syslog_pipeline_tcp.erl
%% syslog_pipeline_tcp entry point
%%
-module (syslog_pipeline_tcp).

-export([start_server/4]).
-export([stop_listener/1]).
-export([start_link/4]).
-export([init/4]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, Pipeline) when is_integer(NbAcceptors), NbAcceptors > 0 ->
  ranch:start_listener(Ref, NbAcceptors, ranch_tcp, TransOpts, ?MODULE, Pipeline).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).

%% @doc Start an syslog protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Pipeline) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Pipeline]),
  {ok, Pid}.

%% Internal.
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Pipeline) ->
  ok = ranch:accept_ack(ListenerPid),
  recv(<<>>, Socket, Transport, Pipeline).

%% @private
-spec recv(binary(), inet:socket(), module(), any()) -> ok.
recv(<<>>, Socket, Transport, Pipeline) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer = syslog_pipeline:handle(Pipeline, Data),
      recv(Buffer, Socket, Transport, Pipeline);
    {error, _}->
      terminate(Socket, Transport)
  end;
recv(Buffer, Socket, Transport, Pipeline) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer2 = syslog_pipeline:handle(Pipeline, <<Buffer/binary, Data/binary>>),
      recv(Buffer2, Socket, Transport, Pipeline);
    {error, _}->
      terminate(Socket, Transport)
  end.

%% @private
-spec terminate(inet:socket(), module()) -> ok.
terminate(Socket, Transport)->
  Transport:close(Socket),
  ok.
