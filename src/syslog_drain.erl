%%
%% syslog_drain.erl
%% syslog_drain entry point
%%
-module (syslog_drain).

-export([start_server/4]).
-export([stop_listener/1]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, ProtoOpts)
    when is_integer(NbAcceptors), NbAcceptors > 0 ->
  ranch:start_listener(Ref, NbAcceptors,
    ranch_tcp, TransOpts, syslog_drain_protocol, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).
