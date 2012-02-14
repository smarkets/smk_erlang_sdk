-module(smk_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("smk_tests.hrl").

-define(setup(F), {setup, fun setup/0, F}).
-define(MARKET_ID, #seto_uuid_128{low=122001}).
-define(CONTRACT_ID, #seto_uuid_128{low=175001}).

setup() ->
  application:load(smk),
  %application:set_env(smk, host, "api-dev.corp.smarkets.com"),
  %application:set_env(smk, port, 3701),
  application:set_env(smk, host, "localhost"),
  application:set_env(smk, port, 3700),
  application:set_env(smk, ssl, false),
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  %application:start(lager),
  application:start(smk).

login_test_() -> ?setup(
    {timeout, 10, fun() ->
        {ok, C} = login(),
        ?assertLoginResponse(1),
        {ok, 2} = smk_client:logout(C),
        ?assertLogoutConfirmation(2)
    end}
  ).

account_state_exposure_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),

  {ok, 2} = smk_client:account_state(C),
  ?assertAccountState(2, #seto_account_state{
      exposure = #seto_decimal{value=0, exponent=2}
    }),

  Qty = 100000,
  Px = 2500,
  Side = buy,
  {ok, 3} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
  ?assertOrderAccepted(3, Order, 3),

  {ok, 4} = smk_client:account_state(C),
  ?assertAccountState(4, #seto_account_state{
      exposure = #seto_decimal{value=-250, exponent=2}
    }),

  {ok, 5} = smk_client:order_cancel(C, Order),
  ?assertOrderCancelled(5, Order, member_requested),

  {ok, 6} = smk_client:account_state(C),
  ?assertAccountState(6, #seto_account_state{
      exposure = #seto_decimal{value=0, exponent=2}
    }),

  {ok, 7} = smk_client:logout(C),
  ?assertLogoutConfirmation(7).

unauthorised_test() ->
  UserCreds = [
    {username, <<"not a valid username">>},
    {password, <<"not a valid password">>}
  ],
  Pid = self(),
  Callback = fun(Payload,Session) ->
      cb(Pid, Payload, Session)
  end,
  smk_clients_sup:start_client([
      {callback, Callback},
      {backoff, false}
      |UserCreds
    ]),
  ?assertLogout(1, unauthorised).

ping_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  lists:foreach(
    fun(I) ->
        I1 = I + 1,
        {ok, I1} = smk_client:ping(C)
    end,
    lists:seq(1, 10)
  ),
  lists:foreach(
    fun(I) -> I1 = I+1, ?assertPong(I1) end,
    lists:seq(1, 10)
  ),
  {ok, 12} = smk_client:logout(C),
  ?assertLogoutConfirmation(12).

ping_replay_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  ok = smk_client:drop_in(C),
  {ok, 2} = smk_client:ping(C),
  % in 3 dropped
  {ok, 3} = smk_client:ping(C),
  % out 5 replay
  ?assertPong(2),
  ?assertPong(3),
  {ok, 5} = smk_client:logout(C),
  ?assertLogoutConfirmation(4).

resume_test_() ->
  {timeout, 15, fun() ->
        Name = resume_test,
        {ok, _} = login(Name),
        ?assertLoginResponse(1, Session),
        exit(whereis(Name), kill),
        ?assertLoginResponse(2, Session),
        {ok, 3} = smk_client:logout(Name),
        ?assertLogoutConfirmation(3)
    end}.

ping_resume_replay_test_() ->
  {timeout, 15, fun() ->
        Name = ping_resume_replay_test,
        {ok, _} = login(Name),
        ?assertLoginResponse(1, Session),
        smk_client:drop_in(Name),
        {ok, 2} = smk_client:ping(Name),
        exit(whereis(Name), kill),
        ?assertLoginResponse(3, Session),
        ?assertPongReplay(2),
        ?assertGapfill(3),
        % outgoing 4 is a replay
        {ok, 5} = smk_client:logout(Name),
        ?assertLogoutConfirmation(4)
    end}.

order_create_test_() ->
  {timeout, 15, fun() ->
        {ok, C} = login(),
        ?assertLoginResponse(1),
        Qty = 100000,
        Px = 2500,
        Side = buy,
        {ok, 2} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
        ?assertOrderAccepted(2, Order, 2),
        {ok, 3} = smk_client:order_cancel(C, Order),
        ?assertOrderCancelled(3, Order, member_requested),
        {ok, 4} = smk_client:logout(C),
        ?assertLogoutConfirmation(4)
    end}.

order_rejected_market_not_found_test_() ->
  {timeout, 15, fun() ->
        {ok, C} = login(),
        ?assertLoginResponse(1),
        Qty = 100000,
        Px = 2500,
        Side = buy,
        {ok, 2} = smk_client:order(C, Qty, Px, Side, #seto_uuid_128{low=99999999}, ?CONTRACT_ID),
        ?assertOrderRejected(2, market_not_found, 2),
        {ok, 3} = smk_client:logout(C),
        ?assertLogoutConfirmation(3)
    end}.

order_rejected_contract_not_found_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  Qty = 100000,
  Px = 2500,
  Side = buy,
  {ok, 2} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, #seto_uuid_128{low=0}),
  ?assertOrderRejected(2, contract_not_found, 2),
  {ok, 3} = smk_client:logout(C),
  ?assertLogoutConfirmation(3).



many_order_create_test() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  Qty = 100000,
  Px = 3000,
  Side = buy,
  lists:foreach(
    fun(I) ->
      I1 = I + 1,
      {ok, I1} = smk_client:order(C, Qty, Px+(I*100), Side, ?MARKET_ID, ?CONTRACT_ID)
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
      {ok, _} = smk_client:order_cancel(C, Order)
    end, Orders
  ),

  all(Orders,
    fun(Order, Recv) ->
      ?orderCancelled(_, Order, member_requested) = Recv
    end
  ),
  {ok, 22} = smk_client:logout(C),
  ?assertLogoutConfirmation(22).

market_subscription_test_() ->
  {timeout, 20, fun() ->
  {ok, C} = login(),
  ?assertLoginResponse(1),
  {ok, 2} = smk_client:subscribe(C, ?MARKET_ID), 
  ?assertMarketQuotes(2, ?MARKET_ID),%, ContractQuotes),
  Qty = 100000,
  Px = 2500,
  Side = buy,
  {ok, 3} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
  ?assertContractQuotes(3, ?CONTRACT_ID),
  
  ?assertOrderAccepted(4, Order, 3),
  {ok, 4} = smk_client:order_cancel(C, Order),
  ?assertContractQuotes(5, ?CONTRACT_ID),
  ?assertOrderCancelled(6, Order, member_requested),

  {ok, 5} = smk_client:logout(C),
  ?assertLogoutConfirmation(7) end}.

order_executed_test_() ->
  Qty = 50000,
  Px = 2500,
  {inparallel, [
      {timeout, 20, fun() ->
        {ok, C} = login_with_user(1),
        ?assertLoginResponse(1),
        Side = buy,
        {ok, 2} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
        ?assertOrderAccepted(2, Order, 2),
        ?assertOrderExecuted(3, Order, Qty, Px),
        {ok, 3} = smk_client:logout(C),
        ?assertLogoutConfirmation(4)
      end},
    {timeout, 20, fun() ->
        {ok, C} = login_with_user(2),
        ?assertLoginResponse(1),
        Side = sell,
        {ok, 2} = smk_client:order(C, Qty, Px, Side, ?MARKET_ID, ?CONTRACT_ID),
        ?assertOrderAccepted(2, Order, 2),
        ?assertOrderExecuted(3, Order, Qty, Px),
        {ok, 3} = smk_client:logout(C),
        ?assertLogoutConfirmation(4)
      end}
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
      {callback, Callback},
      {backoff, false}
      |UserCreds
    ]).
login_with_user(Name, User) ->
  UserCreds = user_creds(User),
  Pid = self(),
  Callback = fun(Payload,Session) ->
      cb(Pid, Payload, Session)
  end,
  smk_clients_sup:start_client({local, Name}, [
      {callback, Callback},
      {backoff, false}
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
