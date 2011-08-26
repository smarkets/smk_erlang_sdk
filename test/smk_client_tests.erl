-module(smk_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("smk_tests.hrl").

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
  ?assertOrderAccepted(2, Order, 2),
  ok = smk_client:order_cancel(C, Order),
  ?assertOrderCancelled(3, Order, member_requested),
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
  Orders =
    lists:map(
      fun(I) ->
        I1 = I + 1,
        ?assertOrderAccepted(I1, Order, I1),
        Order
      end,
      lists:seq(1, 10)
    ),
  lists:foreach(
    fun(Order) ->
      ok = smk_client:order_cancel(C, Order)
    end, Orders
  ),

  all(Orders,
    fun(Order, Recv) ->
      ?orderCancelled(_, Order, member_requested) = Recv
    end
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
  
  ?assertOrderAccepted(4, Order, 3),
  ok = smk_client:order_cancel(C, Order),
  ?assertContractQuotes(5, ?CONTRACT_ID),
  ?assertOrderCancelled(6, Order, member_requested),

  ok = smk_client:logout(C),
  ?assertLogoutConfirmation(7).

order_executed_test_() ->
  Qty = 50000,
  Px = 2500,
  {inparallel, [
      fun() ->
        {ok, C} = login_with_user(1),
        ?assertLoginResponse(1),
        Side = buy,
        ok = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
        ?assertOrderAccepted(2, Order, 2),
        ?assertOrderExecuted(3, Order, Qty, Px),
        ok = smk_client:logout(C),
        ?assertLogoutConfirmation(4)
      end,
      fun() ->
        {ok, C} = login_with_user(2),
        ?assertLoginResponse(1),
        Side = sell,
        ok = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
        ?assertOrderAccepted(2, Order, 2),
        ?assertOrderExecuted(3, Order, Qty, Px),
        ok = smk_client:logout(C),
        ?assertLogoutConfirmation(4)
      end
    ]}.

cb(Pid, Payload, _Session) ->
  Pid ! Payload,
  ok.

login() ->
  Users = users(),
  login_with_user(
    lists:nth(random:uniform(length(Users)), Users)
  ).
login(Name) ->
  Users = users(),
  login_with_user(
    Name,
    lists:nth(random:uniform(length(Users)), Users)
  ).

login_with_user(User) ->
  UserCreds = user_creds(User),
  Pid = self(),
  Callback = fun(Payload,Session) ->
      cb(Pid, Payload, Session)
  end,
  smk_clients_sup:start_client([
      {callback, Callback}
      |UserCreds
    ]).
login_with_user(Name, User) ->
  UserCreds = user_creds(User),
  Pid = self(),
  Callback = fun(Payload,Session) ->
      cb(Pid, Payload, Session)
  end,
  smk_clients_sup:start_client({local, Name}, [
      {callback, Callback}
      |UserCreds
    ]).

user_creds(N) when is_integer(N) ->
  lists:nth(N, users());
user_creds(L) when is_list(L) ->
  L.

users() ->
  {ok, IO} = file:open("../../../test-data/test_usernames.txt", [read]),
  read_users(IO, []).

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
