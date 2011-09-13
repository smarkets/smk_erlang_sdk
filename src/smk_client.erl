-module(smk_client).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).

-define(SERVER, ?MODULE).
-define(HEARTBEAT_TIMEOUT, 10000).
-define(MAX_BACKOFF, ?HEARTBEAT_TIMEOUT).
-define(SOCK_OPTS, [
    binary,
    {active, once},
    {packet, 0},
    {recbuf, 8192},
    {send_timeout, 5000},
    {send_timeout_close, true},
    {nodelay, true}
  ]).

-include("eto_piqi.hrl").
-include("seto_piqi.hrl").
-include("smk_sock.hrl").

-type name() :: atom() | pid().
-type state_name() :: awaiting_session | logged_in | logging_out.
-type from() :: {pid(),reference()}.
-type opt() ::
    {username, binary()}
  | {password, binary()}
  | {callback, fun()}.
-type opts() :: list(opt()).
-type send_response() ::
    {ok, eto_seq()}
  | {error, inet:posix()}.


-record(s, {
    sock,
    heartbeat_ref :: timer:tref(),
    drop_in = 0 :: non_neg_integer(),
    buf = eto_frame:buf(),
    out :: pos_integer(),
    in :: pos_integer(),
    cache :: atom(),
    callback :: function(),
    name :: atom(),
    session :: undefined | binary(),
    logout_reason :: eto_logout_reason()
  }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, start_link/3, stop/1, drop_in/1]).

%% send message
-export([logout/1, ping/1, order/6, order_cancel/2]).
-export([subscribe/2, unsubscribe/2, market_quotes_request/2]).
-export([payload/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([
    init/1, handle_event/3, handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4
  ]).

-export([awaiting_session/3, logged_in/3, logging_out/3]).
-export([awaiting_session/2, logged_in/2, logging_out/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Cache, Opts) ->
  gen_fsm:start_link(?MODULE, [Cache,Opts], []).

start_link(Cache, Name, Opts) ->
  gen_fsm:start_link(Name, ?MODULE, [Cache,Opts], []).

-spec logout(name()) -> send_response().
logout(Name) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{
        type=logout,
        logout=#eto_logout{reason=none}
      },
      type=eto
    }).

-spec stop(name()) -> ok.
stop(Name) ->
  gen_fsm:send_event(Name, stop).

-spec drop_in(name()) -> ok.
drop_in(Name) ->
  gen_fsm:send_event(Name, drop_in).

-spec ping(name()) -> send_response().
ping(Name) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{type=ping},
      type=eto
    }).

-spec order(
  name(),
  non_neg_integer(), non_neg_integer(), seto_side(),
  seto_market(), seto_contract()
) -> send_response().
order(Name, Qty, Px, Side, Mkt, C) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{},
      type=order_create,
      order_create=#seto_order_create{
        type=limit,
        market=Mkt, contract=C, side=Side,
        quantity_type=payoff_currency,
        quantity=Qty,
        price_type=percent_odds,
        price=Px
      }}).

-spec order_cancel(name(), seto_order()) -> send_response().
order_cancel(Name, Order) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{},
      type=order_cancel,
      order_cancel=#seto_order_cancel{
        order=Order
      }}).

-spec subscribe(name(), seto_market()) -> send_response().
subscribe(Name, Mkt) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{},
      type=market_subscribe,
      market_subscribe=#seto_market_subscribe{
        market=Mkt
      }}).

-spec unsubscribe(name(), seto_market()) -> send_response().
unsubscribe(Name, Mkt) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{},
      type=market_unsubscribe,
      market_unsubscribe=#seto_market_unsubscribe{
        market=Mkt
      }}).

-spec market_quotes_request(name(), seto_market()) -> send_response().
market_quotes_request(Name, Mkt) ->
  gen_fsm:sync_send_event(Name,
    #seto_payload{
      eto_payload=#eto_payload{},
      type=market_quotes_request,
      market_quotes_request=#seto_market_quotes_request{
        market=Mkt
      }}).

-spec payload(name(), seto_payload()) -> send_response().
payload(Name, Payload) ->
  gen_fsm:sync_send_event(Name, Payload).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-type init_opt() :: atom() | opts().

-spec init(list(init_opt())) -> {ok, state_name(), #s{}} | {stop, normal}.
init([Cache, Opts]) ->
  Name =
    case process_info(self(), registered_name) of
      {registered_name, N} -> N;
      [] -> self() %% temporary client case
    end,
  {Session,T,LastIn,LastOut} = Cache:session_state(Name),
  lager:info("LastIn ~p LastOut ~p", [LastIn, LastOut]),
  case proplists:get_value(callback, Opts) of
    undefined ->
      lager:error("callback not defined"),
      {stop, normal};
    Callback ->
      Backoff = backoff(T),
      lager:info("connection backoff ~p, cache ~p", [Backoff, Cache]),
      timer:send_after(Backoff, {connect, Opts}),
      {ok, awaiting_session, #s{
            session=Session,
            name=Name,
            in=LastIn+1, out=LastOut+1,
            cache=Cache,
            callback=Callback
          }}
  end.

-spec handle_event(any(), state_name(), #s{}) ->
  {next_state, state_name(), #s{}}.
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

-spec handle_sync_event(any(), from(), state_name(), #s{}) ->
  {next_state, state_name(), #s{}}.
handle_sync_event(_Event, _From, StateName, State) ->
  {next_state, StateName, State}.

handle_info({connect, Opts}, StateName, #s{session=Session, cache=Cache, name=Name} = State) ->
  Login = #seto_payload{
    eto_payload=
        case Session of
          undefined -> #eto_payload{type=login};
          _ ->
            #eto_payload{
              type=login,
              login=#eto_login{session=Session}
            }
        end,
    type=login,
    login=#seto_login{
      username=proplists:get_value(username, Opts),
      password=proplists:get_value(password, Opts)
    }
  },
  Host =
    case application:get_env(smk, host) of
      undefined   -> "api-sandbox.smarkets.com";
      {ok, Host0} -> Host0
    end,
  Port =
    case application:get_env(smk, port) of
      undefined   -> 3701;
      {ok, Port0} -> Port0
    end,
  Ssl =
    case application:get_env(smk, ssl) of
      {ok, false} ->
        false;
      _ ->
        true
    end,
  Cache:connecting(Name),
  {ok, Sock} = smk_sock:connect(Ssl, Host, Port, ?SOCK_OPTS, []),
  {ok, _, NewState} = send_call(Login, State#s{sock=Sock}),
  {next_state, StateName, NewState};

handle_info({heartbeat_timeout, _}, awaiting_session = StateName, State) ->
  {next_state, StateName, State#s{heartbeat_ref=undefined}};
handle_info({heartbeat_timeout, Seq}, StateName, #s{out=Seq} = State0) ->
  {ok, _, State} = send_call(
    #seto_payload{type=eto,eto_payload=#eto_payload{type=heartbeat}},
    State0#s{heartbeat_ref=undefined}),
  {next_state, StateName, State};
handle_info({heartbeat_timeout, _}, StateName, State) ->
  {next_state, StateName, State};

handle_info({_, _, Data} = Msg, StateName, #s{buf=Buf, sock=Sock} = State)
  when ?sock_data(Msg) ->
  smk_sock:setopts(Sock, [{active,once}]),
  Buf1 = eto_frame:buf_append(Buf, Data),
  {NewBuf, Payloads} = deframe_all(Buf1, []),
  {NewStateName, NewState} =
    lists:foldl(
      fun
        (PayloadData, {AccStateName, #s{drop_in=Drop} = AccState}) when Drop > 0 ->
          #seto_payload{
            eto_payload=#eto_payload{seq=Seq}
          } = seto_piqi:parse_payload(PayloadData),
          lager:log(warning, self(), "dropping incoming ~p", [Seq]),
          {AccStateName, AccState#s{drop_in=Drop-1}};
        (PayloadData, {AccStateName, AccState}) ->
          handle_eto(seto_piqi:parse_payload(PayloadData), AccStateName, AccState)
      end,
      {StateName, State}, Payloads),
  {next_state, NewStateName, NewState#s{buf=NewBuf}};

handle_info(Msg, logging_out, #s{logout_reason=undefined, sock=Sock} = State)
  when ?sock_closed(Msg) ->
  lager:notice("logging_out: tcp_closed logout_reason=undefined ~p", [Sock]),
  {stop, normal, State};

handle_info(Msg, logging_out, #s{logout_reason=Reason, sock=Sock} = State)
    when Reason =:= confirmation; Reason =:= login_timeout; Reason =:= login_not_first_seq;
    ?sock_closed(Msg) ->
  lager:notice("logging_out: tcp_closed stopping due to logout_reason=~p ~p", [Reason, Sock]),
  {stop, normal, State};

handle_info(Msg, logging_out, #s{logout_reason=Reason, sock=Sock} = State)
  when ?sock_closed(Msg) ->
  lager:notice("logging_out: tcp_closed while logout_reason=~p ~p", [Reason, Sock]),
  {stop, {logging_out, Reason}, State};

handle_info(Msg, StateName, State=#s{sock=Sock}) when ?sock_closed(Msg) ->
  lager:notice("~p: tcp_closed ~p", [StateName, Sock]),
  {stop, {tcp_error, closed}, State};

handle_info({_, _, Reason} = Msg, StateName, State=#s{sock=Sock}) when ?sock_error(Msg) ->
  lager:error("~p: tcp_error ~p ~p", [StateName, Sock, Reason]),
  {stop, {tcp_error, Reason}, State};

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% state names
%% ------------------------------------------------------------------

%% sync

awaiting_session(_Payload, _From, State0) ->
  {reply, {error, no_session}, awaiting_session, State0}.

-spec logged_in(seto_payload(), from(), #s{}) ->
  {reply, send_response(), logged_in, #s{}}.
logged_in(Payload, _From, State0) ->
  case send_call(Payload, State0) of
    {ok, Seq, State} ->
      {reply, {ok, Seq}, logged_in, State};
    {Error, State} ->
      {reply, Error, logged_in, State}
  end.

logging_out(_Message, _From, State0) ->
  {reply, {error, logging_out}, logging_out, State0}.

%% async

awaiting_session(stop, State) ->
  {next_state, awaiting_session, State}.

logged_in(drop_in, #s{drop_in=Drop} = State) ->
  {next_state, logged_in, State#s{drop_in=Drop+1}};

logged_in(stop, State0) ->
  {ok, _, State} = send_call(
    #seto_payload{type=eto, eto_payload=#eto_payload{type=logout,logout=#eto_logout{reason=none}}},
    State0),
  {next_state, logged_in, State}.

logging_out(stop, State) ->
  {next_state, logging_out, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec handle_eto(seto_payload(), state_name(), #s{}) ->
  {state_name, #s{}}.
handle_eto(#seto_payload{
      eto_payload=#eto_payload{seq=Seq, type=replay}} = Payload,
      StateName,
      #s{in=InSeq} = State
    ) when Seq > InSeq ->
  handle_payload(Payload, StateName, State);
handle_eto(#seto_payload{
      eto_payload=#eto_payload{seq=Seq}}=Payload,
      StateName,
      #s{in=InSeq} = State
    ) when Seq > InSeq ->
  {NewStateName, State0} = login_payload(Payload, StateName, State),
  case {StateName, NewStateName} of
    {awaiting_session, logged_in} ->
      #s{session=Session, callback=Callback} = State0,
      ok = Callback(Payload, Session);
    _ ->
      ok
  end,
  lager:info("~p: Received out of sequence ~p~n", [StateName, Payload]),
  ReplayPayload = #seto_payload{
    type=eto,
    eto_payload=#eto_payload{
      type=replay,
      replay=#eto_replay{seq=InSeq}
    }},
  {ok, _, NewState} = send_call(ReplayPayload, State0),
  {NewStateName, NewState};

handle_eto(#seto_payload{
              eto_payload=#eto_payload{
                seq=Seq,
                logout=#eto_logout{reason=Reason}
              }} = Payload,
            _StateName,
            #s{in=Seq} = State) ->
  #s{
    name=Name,
    session=Session,
    callback=Callback,
    cache=Cache
  } = State,
  Cache:log_in(Name, Seq),
  ok = Callback(Payload, Session),
  {logging_out, State#s{logout_reason=Reason, in=Seq+1}};
handle_eto(#seto_payload{eto_payload=#eto_payload{seq=Seq, type=heartbeat}},
            StateName,
            #s{in=Seq, cache=Cache, name=Name} = State) ->
  Cache:log_in(Name, Seq),
  {StateName, State#s{in=Seq+1}};
handle_eto(#seto_payload{eto_payload=#eto_payload{seq=Seq}} = Payload,
            StateName,
            #s{in=Seq} = State) ->
  #s{
    name=Name,
    session=Session,
    callback=Callback,
    cache=Cache
  } = State,
  lager:info("~p: Received ~p~n", [StateName, Payload]),
  {NewStateName, NewState} = handle_payload(Payload, StateName, State#s{in=Seq+1}),
  Cache:log_in(Name, Seq),
  ok = Callback(Payload, Session),
  {NewStateName, NewState};
handle_eto(_, StateName, State) ->
  {StateName, State}.

-spec handle_payload(seto_payload(), state_name(), #s{}) ->
  {state_name(), #s{}}.
handle_payload(#seto_payload{eto_payload=#eto_payload{replay=#eto_replay{seq=Seq}}},
                StateName,
                #s{name=Name, sock=Sock, cache=Cache} = State) ->
  Cache:map_from(Name, Seq,
    fun(Payload) ->
      sock_send(Sock, replay_payload(Payload))
    end),
  {StateName, State};

handle_payload(#seto_payload{
                  eto_payload=#eto_payload{type=login_response}
                } = Message,
                StateName,
                State) ->
  login_payload(Message, StateName, State);

handle_payload(_Message, StateName, State) ->
  {StateName, State}.

-spec login_payload(seto_payload(), state_name(), #s{}) ->
  {state_name(), #s{}}.
login_payload(
  #seto_payload{
    eto_payload=#eto_payload{
      login_response=#eto_login_response{
        session=Session,
        reset=Reset
      }}}, _StateName, State) ->
  #s{name=Name, cache=Cache, heartbeat_ref=Heartbeat} = State,
  Cache:takeover_session(Name, Session),
  {logged_in, State#s{session=Session, out=Reset, heartbeat_ref=heartbeat_timer(Heartbeat, Reset)}};
login_payload(_, StateName, State) ->
  {StateName, State}.

-spec send_call(seto_payload(), #s{}) ->
  {ok, eto_seq(), #s{}} | {{error, any()}, #s{}}.
send_call(#seto_payload{eto_payload=Eto}=Payload0, State) ->
  #s{
    name=Name,
    out=Seq,
    sock=Sock,
    cache=Cache,
    heartbeat_ref=Ref
  } = State,
  Payload = Payload0#seto_payload{eto_payload=Eto#eto_payload{seq=Seq}},
  case Payload of
    #seto_payload{eto_payload=#eto_payload{type=heartbeat}} -> ok;
    _ -> lager:info("sending: ~p ~p", [Seq, Payload])
  end,
  case sock_send(Sock, Payload) of
    ok ->
      Cache:log_out(Name, Payload),
      Out = Seq+1,
      {ok, Seq, State#s{out=Out, heartbeat_ref=heartbeat_timer(Ref, Out)}};
    Error ->
      {Error, State}
  end.

-spec sock_send(any(), seto_payload()) -> ok | {error, any()}.
sock_send(Sock, Payload) ->
  smk_sock:send(Sock, eto_frame:frame(seto_piqi:gen_payload(Payload))).

-spec replay_payload(seto_payload()) -> seto_payload().
replay_payload(#seto_payload{eto_payload=#eto_payload{type=replay, seq=Seq}}) ->
  gapfill(Seq);
replay_payload(#seto_payload{eto_payload=#eto_payload{type=login, seq=Seq}}) ->
  gapfill(Seq);
replay_payload(#seto_payload{eto_payload=Eto} = Payload) ->
  Payload#seto_payload{eto_payload=Eto#eto_payload{is_replay=true}}.

-spec gapfill(eto_seq()) -> seto_payload().
gapfill(Seq) ->
  #seto_payload{
    type=eto,
    eto_payload=#eto_payload{
      type=gapfill,
      is_replay=true,
      seq=Seq
    }}.

-spec deframe_all(eto_frame:buf(), list(binary())) ->
  {eto_frame:buf(), list(binary())}.
deframe_all(Buf, Acc) ->
    case eto_frame:deframe(Buf) of
        {PayloadData, Buf1} ->
            deframe_all(Buf1, [PayloadData|Acc]);
        Buf1 -> {Buf1, lists:reverse(Acc)}
    end.

-spec heartbeat_timer(undefined | timer:tref(), eto_seq()) -> timer:tref().
heartbeat_timer(undefined, Seq) ->
  {ok, Ref} = timer:send_after(?HEARTBEAT_TIMEOUT, {heartbeat_timeout, Seq}),
  Ref;
heartbeat_timer(Ref, Seq) ->
  timer:cancel(Ref),
  heartbeat_timer(undefined, Seq).

-spec backoff(undefined | {pos_integer(),pos_integer(),pos_integer()}) ->
  pos_integer().
backoff(undefined) ->
  0;
backoff(T) ->
  Diff = timer:now_diff(now(), T),
  Milli = Diff div 1000,
  lager:info("backoff Milli ~p", [Milli]),
  case Milli of
    0 -> 0;
    _ -> erlang:min((?MAX_BACKOFF div Milli) * 1000, ?MAX_BACKOFF)
  end.
