-module(smk).

-export([start/0]).

start() ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  application:start(smk).
