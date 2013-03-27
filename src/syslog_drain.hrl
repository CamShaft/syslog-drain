
-define(PRINT(Var), io:format("~p~n", [Var])).

-record(drain_opts, {
  parsers :: [module()],
  mappers :: [module()],
  emitters :: [module()],
  feedback :: boolean()
}).
