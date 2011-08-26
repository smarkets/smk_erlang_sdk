-module(smk_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("smk_tests.hrl").

-include("eto_piqi.hrl").
-include("seto_piqi.hrl").

-define(setup(F), {setup, fun setup/0, F}).
-define(MARKET_ID, #seto_uuid_128{low=122001}).
-define(CONTRACT_ID, #seto_uuid_128{low=175002}).

setup() ->
  application:load(smk_erlang_sdk),
  application:set_env(smk_erlang_sdk, host, "vagrant-dev.corp.smarkets.com"),
  %application:set_env(smk_erlang_sdk, restart_strategy, temporary),
  %application:start(lager),
  application:start(smk_erlang_sdk).

login_test_() -> ?setup(
    fun() ->
        {ok, C} = login(),
        ?assertLoginResponse(1),
        ok = smk_client:logout(C),
        ?assertLogoutConfirmation(2)
    end
  ).

ping_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  lists:foreach(
    fun(_) -> ok = smk_client:ping(C) end,
    lists:seq(1, 10)
  ),
  lists:foreach(
    fun(I) -> I1 = I+1, ?assertPong(I1) end,
    lists:seq(1, 10)
  ),
  ok = smk_client:logout(C),
  ?assertLogoutConfirmation(12).

resume_test() ->
  {timeout, 15, fun() ->
        Name = resume_test,
        {ok, _} = login(Name),
        ?assertLoginResponse(1, Session),
        exit(whereis(Name), kill),
        ?assertLoginResponse(2, Session),
        ok = smk_client:logout(Name),
        ?assertLogoutConfirmation(3)
    end}.

order_create_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  Qty = 100000,
  Px = 2500,
  Side = buy,
  ok = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
  ?assertOrderAccepted(2, OrderId, 2),
  ok = smk_client:order_cancel(C, OrderId),
  ?assertOrderCancelled(3, OrderId, member_requested),
  ok = smk_client:logout(C),
  ?assertLogoutConfirmation(4).

many_order_create_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  Qty = 100000,
  Px = 3000,
  Side = buy,
  lists:foreach(
    fun(I) ->
      ok = smk_client:order(C, Qty, Px+(I*100), Side, ?MARKET_ID, ?CONTRACT_ID)
    end,
    lists:seq(1, 10)
  ),
  OrderIds =
    lists:map(
      fun(I) ->
        I1 = I + 1,
        ?assertOrderAccepted(I1, OrderId, I1),
        ok = smk_client:order_cancel(C, OrderId),
        {OrderId, I}
      end,
      lists:seq(1, 10)
    ),
  lists:foreach(
    fun({OrderId, I}) ->
      I1 = I + 11,
      ?assertOrderCancelled(I1, OrderId, member_requested)
    end,
    OrderIds
  ),
  ok = smk_client:logout(C),
  ?assertLogoutConfirmation(22).

market_subscription_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  ok = smk_client:subscribe(C, ?MARKET_ID), 
  ?assertMarketQuotes(2, ?MARKET_ID),%, ContractQuotes),
  Qty = 100000,
  Px = 2500,
  Side = buy,
  ok = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
  ?assertContractQuotes(3, ?CONTRACT_ID),
  
  ?assertOrderAccepted(4, OrderId, 3),
  ok = smk_client:order_cancel(C, OrderId),
  ?assertContractQuotes(5, ?CONTRACT_ID),
  ?assertOrderCancelled(6, OrderId, member_requested),

  ok = smk_client:logout(C),
  ?assertLogoutConfirmation(7).

cb(Pid, Payload, _Session) ->
  Pid ! Payload,
  ok.

login() ->
  Pid = self(),
  Callback =
    fun(Payload,Session) ->
      cb(Pid, Payload, Session)
    end,
  smk_clients_sup:start_client([
      {callback, Callback}
      |creds()
    ]).
login(Name) ->
  Pid = self(),
  Callback = fun(Payload,Session) ->
      cb(Pid, Payload, Session)
  end,
  smk_clients_sup:start_client({local, Name}, [
      {callback, Callback}
      |creds()
    ]).

creds() ->
  {ok, IO} = file:open("../USERS", [read]),
  Users = read_users(IO, []),
  lists:nth(random:uniform(length(Users)), Users).

read_users(IO, Users) ->
  case file:read_line(IO) of
    eof -> Users;
    {ok, Line} ->
      [Username,Password] = binary:split(trim(Line), <<$:>>),
      read_users(IO, [[{username,Username},{password,Password}]|Users])
  end.

trim(Input) ->
  [R,_] = re:replace(Input, <<"\\s+">>, <<"">>, [global]),
  R.
