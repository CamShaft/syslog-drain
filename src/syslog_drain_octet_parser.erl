-module(syslog_drain_octet_parser).

-export([parse/1]).

%% http://tools.ietf.org/html/rfc3164#section-4.1
-define(MAX_LENGTH, 1024).

%%%%
%% Iterate through the buffer and gather valid octet frames
%%%%
parse(Buffer) ->
  parse(Buffer, []).

parse(<<>>, Frames)->
  {Frames, <<>>};
parse(Buffer, Frames)->
  case frame(Buffer) of
    {ok, Frame, Rest} ->
      parse(Rest, [Frame|Frames]);
    eos ->
      {Frames, <<>>};
    _ ->
      {Frames, Buffer}
  end.

%%%%
%% Find the next frame in the buffer and analyize it
%%%%
frame(Buffer) ->
  analyze(scan(Buffer, 0)).

%%%%
%% Scan for the next frame
%%%%

scan(<<" ", Rest/binary>>, 0)->
  scan(Rest, 0);
%% We have a space and have gathered some numbers
scan(<<" ", Rest/binary>>, Length) when Length < ?MAX_LENGTH ->
  {ok, Length, Rest};
%% We have a number
scan(<<$0, Rest/binary>>, Length) ->
  scan(Rest, Length*10);
scan(<<$1, Rest/binary>>, Length) ->
  scan(Rest, Length*10+1);
scan(<<$2, Rest/binary>>, Length) ->
  scan(Rest, Length*10+2);
scan(<<$3, Rest/binary>>, Length) ->
  scan(Rest, Length*10+3);
scan(<<$4, Rest/binary>>, Length) ->
  scan(Rest, Length*10+4);
scan(<<$5, Rest/binary>>, Length) ->
  scan(Rest, Length*10+5);
scan(<<$6, Rest/binary>>, Length) ->
  scan(Rest, Length*10+6);
scan(<<$7, Rest/binary>>, Length) ->
  scan(Rest, Length*10+7);
scan(<<$8, Rest/binary>>, Length) ->
  scan(Rest, Length*10+8);
scan(<<$9, Rest/binary>>, Length) ->
  scan(Rest, Length*10+9);
%% We've at the end of the stream and haven't found any numbers
scan(<<>>, 0)  ->
  eos;
%% We've at the end of the stream and we were in the middle of checking numbers
scan(<<>>, _)  ->
  continue;
%% Move on to the next byte, we don't understand this one
scan(<<_, Rest/binary>>, _)->
  scan(Rest, 0).

%%%%
%% analyze the frame
%%%%

%% We are at the end of the stream and have a perfect match
analyze({ok, Length, Rest}) when byte_size(Rest) =:= Length ->
  split(Rest, Length);
%% There's still more in the buffer so we can do some checks
analyze({ok, Length, Rest}) when byte_size(Rest) > Length ->
  %% check the next frame to see if we have a valid frame here
  case split(Rest, Length) of
    %% The next frame starts with a digit; in most cases this means we have
    %% a valid frame
    {ok, Frame, <<$0,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$1,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$2,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$3,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$4,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$5,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$6,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$7,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$8,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};
    {ok, Frame, <<$9,_/binary>>=Rest2} ->
      {ok, Frame, Rest2};

    %% The next "frame" after `Length` doesn't start with a number so we're
    %% probably not looking at a valid frame here; move on by skipping this
    %% `Length`
    _ ->
      {ok, _, Rest2} = split(Rest, byte_size(<<Length>>)+1),
      frame(Rest2)
  end;
%% We need more in the buffer to understand this frame
analyze({ok, _, _}) ->
  continue;
%% Pass on the result if we don't understand it
analyze(Result) ->
  Result.

%%%%
%% Split `Bin` by `Length` in bytes
%%%%
split(Bin, Length)->
  <<First:Length/binary, Second/binary>> = Bin,
  {ok, First, Second}.
