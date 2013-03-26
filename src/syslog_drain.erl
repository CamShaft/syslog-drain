%%
%% syslog_drain.erl
%% syslog_drain entry point
%%
-module (syslog_drain).

-export([start_server/4]).
-export([stop_listener/1]).
-export([set_env/3]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, ProtoOpts)
    when is_integer(NbAcceptors), NbAcceptors > 0 ->
  ranch:start_listener(Ref, NbAcceptors,
    ranch_tcp, TransOpts, syslog_drain, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).

%% @doc Convenience function for setting an environment value.
%%
%% Allows you to update live an environment value; mainly used to
%% add/remove parsers and emitters
-spec set_env(any(), atom(), any()) -> ok.
set_env(Ref, Name, Value) ->
  Opts = ranch:get_protocol_options(Ref),
  {_, Env} = lists:keyfind(env, 1, Opts),
  Env2 = [{Name, Value}|lists:keydelete(Name, 1, Env)],
  Opts2 = lists:keyreplace(env, 1, Opts, {env, Env2}),
  ok = ranch:set_protocol_options(Ref, Opts2).
