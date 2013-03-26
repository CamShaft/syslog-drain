-module(syslog_drain_protocol).

-export([start_link/4, init/4]).

%% @doc Start an syslog protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

%% Internal.

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
  Parsers = get_value(parsers, Opts, []),
  Emitters = get_value(emitters, Opts, []),
  ok = ranch:accept_ack(ListenerPid),
  recv(<<>>, Socket, Transport, Parsers, Emitters).

%% @private
-spec recv(binary(), inet:socket(), module(), list(module()), list(module())) -> ok.
recv(Buffer, Socket, Transport, Parsers, Emitters) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      {Frames, Buffer2} = syslog_drain_octet_parser:parse(<<Buffer/binary, Data/binary>>),
      Events = events(Frames, Parsers, []),
      [catch Emitter:emit(Events) || Emitter <- Emitters],
      recv(Buffer2, Socket, Transport, Parsers, Emitters);
    {error, _}->
      terminate(Socket, Transport)
  end.

%% @private
events(_, [], Events)->
  Events;
events(Frames, [Parser|Parsers], Events)->
  ParsedEvents = parse(Frames, Parser, Events),
  events(Frames, Parsers, ParsedEvents).

%% @private
parse([], _, Events)->
  Events;
parse([Frame|Frames], Parser, Events)->
  case catch Parser:parse(Frame) of
    ParsedEvents when is_list(ParsedEvents) ->
      parse(Frames, Parser, ParsedEvents++Events);
    _ ->
      parse(Frames, Parser, Events)
  end.

%% @private
-spec terminate(inet:socket(), module()) -> ok.
terminate(Socket, Transport)->
  Transport:close(Socket),
  ok.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
