%% @doc Behaviour for parsers.
%%
%% Only one function needs to be implemented, <em>parse/1</em>.
%% It receives a binary frame recieved from a drain.
-module(syslog_drain_parser).

-callback parse(Frame)
  -> list([{binary(), binary()}])
  when Frame::binary().
