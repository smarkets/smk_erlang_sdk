-module(smk_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("smk_tests.hrl").

-define(USERNAME, <<"hunter.morris@smarkets.com">>).
-define(PASSWORD, <<"abc,123">>).

-include("eto_piqi.hrl").
-include("seto_piqi.hrl").

setup() ->
  ?debugMsg("Setup"),
  application:load(smk_erlang_sdk),
  application:set_env(smk_erlang_sdk, host, "vagrant-dev.corp.smarkets.com"),
  application:set_env(smk_erlang_sdk, restart_strategy, temporary),
  application:start(smk_erlang_sdk).

login_test_() ->
  {setup, fun setup/0, [
      fun() ->
        Pid = self(),
        {ok, C} = login(
          fun(Payload,Session) ->
              cb(Pid, Payload, Session)
          end),
        ?assertLoginResponse(1),
        smk_client:logout(C),
        ?assertLogoutConfirmation(2)
      end
  ]}.

cb(Pid, Payload, _Session) ->
  Pid ! Payload,
  ok.

login(Callback) ->
  smk_clients_sup:start_client([
      {username,?USERNAME},
      {password,?PASSWORD},
      {callback, Callback}
    ]).
