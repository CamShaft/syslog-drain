-module(syslog_drain_handler).

-include("syslog_drain.hrl").

-export ([handle/2]).
%% @private
-export ([parse_frame/1]).
-export ([exec_router/3]).

handle(Buffer, _Options=#drain_opts{feedback=true,parsers=Parsers,routes=Routes})->
  {_FrameTime, {Frames, Buffer2}} = timer:tc(syslog_octet_frame, parse, [Buffer]),
  {_ParseTime, Messages} = timer:tc(?MODULE, parse_frame, [Frames]),
  {_ParseBodyTime, ValidMessages} = timer:tc(?MODULE, parse_body, [Messages, Parsers, []]),
  {_RouteTime, _} = timer:tc(?MODULE, exec_router, [Routes, ValidMessages]),
  Buffer2;
handle(Buffer, _Options=#drain_opts{parsers=Parsers,routes=Routes})->
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),
  Messages = parse_frame(Frames),
  ValidMessages = parse_body(Messages, Parsers, []),
  exec_router(Routes, ValidMessages),
  Buffer2.

parse_frame(Frames)->
  [syslog_header:parse(Frame) || Frame <- Frames].

parse_body([], _, ValidMessages)->
  ValidMessages;
parse_body([{ok, Message}|Messages], Parsers, ValidMessages)->
  case exec_parser(syslog_drain:get_value(message, Message, <<>>), Parsers) of
    {ok, MessageFields} ->
      parse_body(Messages, Parsers, [{message_fields,MessageFields}|ValidMessages]);
    _ ->
      parse_body(Messages, Parsers, ValidMessages)
  end;
parse_body([_|Messages], Parsers, ValidMessages)->
  parse_body(Messages, Parsers, ValidMessages).

exec_parser(_, [])->
  {error, not_recognized};
exec_parser(Body, [Parser|Parsers])->
  case catch Parser:parse(Body) of
    {ok, MessageFields} ->
      {ok, MessageFields};
    _ ->
      exec_parser(Body, Parsers)
  end.

exec_router(_, _)->
  ok;
exec_router(Routes, Messages)->
  MatchedRoutes = [syslog_drain_router:apply(Routes, Message) || Message <- Messages, syslog_drain_router:match(Routes, Message)],
  case MatchedRoutes of
    []->
      exec_router(Routes, Messages);
    %% TODO convert 
    MessagesToSend ->
      Emitter:emit(MessagesToSend)
  end.

