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

-include("seto_piqi.hrl").

-record(s, {
    sock,
    heartbeat_ref :: reference(),
    buf = eto_frame:buf(),
    out :: pos_integer(),
    in :: pos_integer(),
    cache :: atom(),
    callback :: function(),
    name :: atom(),
    session :: undefined | binary(),
    logout_reason :: seto_logout_reason()
  }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, stop/1]).

%% send message
-export([ping/1, order/6, order_cancel/2]).
-export([subscribe/2, unsubscribe/2, market_request/2, market_quotes_request/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([awaiting_session/3, logged_in/3, logging_out/3]).
-export([awaiting_session/2, logged_in/2, logging_out/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Cache, Name, Opts) ->
  gen_fsm:start_link(Name, ?MODULE, [Cache,Opts], []).

stop(Name) ->
  gen_fsm:send_event(Name, stop).

ping(Name) ->
  gen_fsm:sync_send_event(Name, ping).

order(Name, Qty, Px, Side, Cg, C) ->
  MessageRec = #seto_order_create{quantity=Qty, price=Px, side=Side, group=Cg, contract=C},
  gen_fsm:sync_send_event(Name, {order_create, MessageRec}).

order_cancel(Name, Order) ->
  MessageRec = #seto_order_cancel{order=Order},
  gen_fsm:sync_send_event(Name, {order_cancel, MessageRec}).

subscribe(Name, Group) ->
  MessageRec = #seto_market_subscription{group=Group},
  gen_fsm:sync_send_event(Name, {market_subscription, MessageRec}).

unsubscribe(Name, Group) ->
  MessageRec = #seto_market_unsubscription{group=Group},
  gen_fsm:sync_send_event(Name, {market_unsubscription, MessageRec}).

market_request(Name, Group) ->
  MessageRec = #seto_market_request{group=Group},
  gen_fsm:sync_send_event(Name, {market_request, MessageRec}).

market_quotes_request(Name, Group) ->
  MessageRec = #seto_market_quotes_request{group=Group},
  gen_fsm:sync_send_event(Name, {market_quotes_request, MessageRec}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Cache, Opts]) ->
  {registered_name, Name} = process_info(self(), registered_name),
  {Session,T,LastIn,LastOut} = Cache:session_state(Name),
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

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {next_state, StateName, State}.

handle_info({connect, Opts}, StateName, #s{session=Session, cache=Cache, name=Name} = State) ->
  Login =
    {login, #seto_login{
        username=proplists:get_value(username, Opts),
        password=proplists:get_value(password, Opts),
        session=Session
      }},
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  Cache:connecting(Name),
  {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),
  {ok, NewState} = send_call(Login, State#s{sock=Sock}),
  {next_state, StateName, NewState};

handle_info({heartbeat_timeout, Seq}, StateName, #s{out=Seq} = State0) ->
  {_, State} = send_call(heartbeat, State0#s{heartbeat_ref=undefined}),
  {next_state, StateName, State};
handle_info({heartbeat_timeout, _}, StateName, State) ->
  {next_state, StateName, State};

handle_info({tcp, Sock, Data}, StateName, #s{buf=Buf} = State) ->
  inet:setopts(Sock, [{active,once}]),
  Buf1 = eto_frame:buf_append(Buf, Data),
  {NewBuf, Payloads} = deframe_all(Buf1, []),
  {NewStateName, NewState} =
    lists:foldl(
      fun(PayloadData, {AccStateName, AccState}) ->
        handle_payload(seto_piqi:parse_payload(PayloadData), AccStateName, AccState)
      end,
      {StateName, State}, Payloads),
  {next_state, NewStateName, NewState#s{buf=NewBuf}};

handle_info({tcp_closed, Sock}, logging_out, #s{logout_reason=undefined} = State) ->
  lager:notice("logging_out: tcp_closed logout_reason=undefined ~p", [Sock]),
  {stop, normal, State};

handle_info({tcp_closed, Sock}, logging_out, #s{logout_reason=comfirmation} = State) ->
  lager:notice("logging_out: tcp_closed logout_reason=comfirmation ~p", [Sock]),
  {stop, normal, State};

handle_info({tcp_closed, Sock}, logging_out, #s{logout_reason=Reason} = State) ->
  lager:notice("logging_out: tcp_closed while logout_reason=~p ~p", [Reason, Sock]),
  {stop, {logging_out, Reason}, State};

handle_info({tcp_closed, Sock}, StateName, State) ->
  lager:notice("~p: tcp_closed ~p", [StateName, Sock]),
  {stop, {tcp_error, closed}, State};

handle_info({tcp_error, Sock, Reason}, StateName, State) ->
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

awaiting_session(_Message, _From, State0) ->
  {reply, {error, no_session}, awaiting_session, State0}.

logged_in(Message, _From, State0) ->
  {Reply, State} = send_call(Message, State0),
  {reply, Reply, logged_in, State}.

logging_out(_Message, _From, State0) ->
  {reply, {error, logging_out}, logging_out, State0}.

%% async

awaiting_session(stop, State) ->
  {next_state, awaiting_session, State}.

logged_in(stop, State0) ->
  {ok, State} = send_call({logout, #seto_logout{}}, State0),
  {next_state, logged_in, State}.

logging_out(stop, State) ->
  {next_state, logging_out, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_payload({sequenced, Sequenced}, StateName, State) ->
  handle_sequenced(Sequenced, StateName, State);
handle_payload(Transient, StateName, #s{callback=Callback, session=Session} = State) ->
  ok = Callback(Transient, Session),
  {StateName, State}.

handle_sequenced(#seto_sequenced{seq=Seq, message={replay, _} = Message}, StateName,
                 #s{in=InSeq} = State) when Seq > InSeq ->
  handle_message(Message, StateName, State);
handle_sequenced(#seto_sequenced{seq=Seq, message=Message}, StateName, #s{in=InSeq} = State) when Seq > InSeq ->
  {NewStateName, State0} = login_message(Message, StateName, State),
  {ok, NewState} = send_call({replay, #seto_replay{seq=InSeq}}, State0),
  {NewStateName, NewState};

handle_sequenced(#seto_sequenced{seq=Seq, message={logout, #seto_logout{reason=Reason}}}, _StateName,
                 #s{in=Seq, cache=Cache, name=Name} = State) ->
  Cache:log_in(Name, Seq),
  {logging_out, State#s{logout_reason=Reason}};
handle_sequenced(#seto_sequenced{seq=Seq, message=heartbeat}, StateName, #s{in=Seq} = State) ->
  {StateName, State#s{in=Seq+1}};
handle_sequenced(#seto_sequenced{seq=Seq, message=Message} = Payload, StateName,
                 #s{in=Seq, name=Name, session=Session, callback=Callback, cache=Cache} = State) ->
  lager:info("~p: Received {sequenced, ~p}~n", [StateName, Payload]),
  {NewStateName, NewState} = handle_message(Message, StateName, State#s{in=Seq+1}),
  Cache:log_in(Name, Seq),
  ok = Callback(Payload, Session),
  {NewStateName, NewState};
handle_sequenced(_, StateName, State) ->
  {StateName, State}.

handle_message({replay, #seto_replay{seq=Seq}}, StateName, #s{name=Name, sock=Sock, cache=Cache} = State) ->
  Cache:map_from(Name, Seq,
    fun(Payload) ->
      sock_send(Sock, replay_payload(Payload))
    end),
  {StateName, State};

handle_message({login_response, _} = Message, StateName, State) ->
  login_message(Message, StateName, State);

handle_message(_Message, StateName, State) ->
  {StateName, State}.

login_message({login_response, #seto_login_response{session=Session, reset=Reset}}, _StateName, State) ->
  #s{name=Name, cache=Cache} = State,
  Cache:takeover_session(Name, Session),
  {logged_in, State#s{session=Session, out=Reset}};
login_message(_, StateName, State) ->
  {StateName, State}.

send_call(Message, #s{name=Name, out=Seq, sock=Sock, cache=Cache, heartbeat_ref=Ref} = State) ->
  case Message of
    heartbeat -> ok;
    _ -> lager:info("sending: ~p ~p", [Seq, Message])
  end,
  Payload = #seto_sequenced{seq=Seq, message=Message},
  case sock_send(Sock, Payload) of
    ok ->
      Cache:log_out(Name, Payload),
      Out = Seq+1,
      {ok, State#s{out=Out, heartbeat_ref=heartbeat_timer(Ref, Out)}};
    Error ->
      {Error, State}
  end.

sock_send(Sock, Payload) ->
  gen_tcp:send(Sock, eto_frame:frame(seto_piqi:gen_payload({sequenced, Payload}))).

replay_payload(#seto_sequenced{message={replay,_}, seq=Seq}) ->
  gapfill(Seq);
replay_payload(#seto_sequenced{message={login,_}, seq=Seq}) ->
  gapfill(Seq);
replay_payload(Payload) ->
  Payload#seto_sequenced{replay=true}.

gapfill(Seq) ->
  #seto_sequenced{message=gapfill, replay=true, seq=Seq}.

deframe_all(Buf, Acc) ->
    case eto_frame:deframe(Buf) of
        {PayloadData, Buf1} ->
            deframe_all(Buf1, [PayloadData|Acc]);
        Buf1 -> {Buf1, lists:reverse(Acc)}
    end.

heartbeat_timer(undefined, Seq) ->
  {ok, Ref} = timer:send_after(?HEARTBEAT_TIMEOUT, {heartbeat_timeout, Seq}),
  Ref;
heartbeat_timer(Ref, Seq) ->
  timer:cancel(Ref),
  heartbeat_timer(undefined, Seq).

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
