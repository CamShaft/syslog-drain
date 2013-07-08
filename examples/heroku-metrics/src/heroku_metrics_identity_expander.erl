-module(heroku_metrics_identity_expander).

-export([expand/1]).

expand(Message)->
  [Message].
