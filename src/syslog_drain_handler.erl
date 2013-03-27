-module(syslog_drain_handler).

-include("syslog_drain.hrl").

-export ([handle/2]).

handle(Buffer, _Options)->
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),
  _Messages = [syslog_header:parse(Frame) || Frame <- Frames],
  Buffer2.
