-module(smk_my_callbacks).

-export([start/2, start_unnamed/2, cb/2]).

start(Username, Password) ->
  smk_clients_sup:start_client({local, ?MODULE}, [
      {username,Username},
      {password,Password},
      {callback, fun ?MODULE:cb/2}
    ]).

start_unnamed(Username, Password) ->
  smk_clients_sup:start_client([
      {username,Username},
      {password,Password},
      {callback, fun ?MODULE:cb/2}
    ]).

cb(_Payload, _Session) ->
  %io:format("Payload Received for session ~p : ~p~n", [Session, Payload]),
  ok.
