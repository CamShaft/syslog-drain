%%
%% syslog_drain.erl
%% syslog_drain entry point
%%
-module (syslog_drain).

-export([start_server/4]).
-export([stop_listener/1]).
-export([start_link/4]).
-export([init/4]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, Opts) when is_integer(NbAcceptors), NbAcceptors > 0 ->
  NumWorkers = proplists:get_value(num_workers, Opts, 10),
  BodyParser = proplists:get_value(body_parser, Opts),
  Emitters = proplists:get_value(emitters, Opts, []),
  {ok, _} = syslog_pipeline:start_pipeline(Ref, NumWorkers, BodyParser, Emitters),
  ranch:start_listener(Ref, NbAcceptors, ranch_tcp, TransOpts, ?MODULE, Ref).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).

%% @doc Start an syslog protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Ref) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Ref]),
  {ok, Pid}.

%% Internal.
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Ref) ->
  ok = ranch:accept_ack(ListenerPid),
  recv(<<>>, Socket, Transport, Ref).

%% @private
-spec recv(binary(), inet:socket(), module(), any()) -> ok.
recv(<<>>, Socket, Transport, Ref) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer = syslog_pipeline:handle(Ref, Data),
      recv(Buffer, Socket, Transport, Ref);
    {error, _}->
      terminate(Socket, Transport)
  end;
recv(Buffer, Socket, Transport, Ref) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      Buffer2 = syslog_pipeline:handle(Ref, <<Buffer/binary, Data/binary>>),
      recv(Buffer2, Socket, Transport, Ref);
    {error, _}->
      terminate(Socket, Transport)
  end.

%% @private
-spec terminate(inet:socket(), module()) -> ok.
terminate(Socket, Transport)->
  Transport:close(Socket),
  ok.
