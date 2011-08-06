-module(smk_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(HEARTBEAT_TIMEOUT, 10000).
-define(SOCK_OPTS, [
    binary,
    {active, once},
    {packet, 0},
    {recbuf, 8192},
    {send_timeout, 5000},
    {send_timeout_close, true},
    {nodelay, true}
  ]).

-record(s, {
    sock,
    heartbeat_ref :: reference(),
    buf = eto_frame:buf(),
    out :: pos_integer(),
    in :: pos_integer(),
    cache :: atom(),
    callback :: function(),
    name :: atom(),
    session :: undefined | binary()
  }).

-include("seto_piqi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, stop/1, seq/1]).

%% send message
-export([ping/1, order/6, order_cancel/2]).
-export([subscribe/2, unsubscribe/2, market_request/2, market_quotes_request/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Cache, Name, Opts) ->
  gen_server:start_link(Name, ?MODULE, [Cache,Opts], []).

stop(Name) ->
  gen_server:cast(Name, stop).

seq(Name) ->
  gen_server:call(Name, seq).

ping(Name) ->
  gen_server:call(Name, ping).

order(Name, Qty, Px, Side, Cg, C) ->
  MessageRec = #seto_order_create{quantity=Qty, price=Px, side=Side, group=Cg, contract=C},
  gen_server:call(Name, {order_create, MessageRec}).

order_cancel(Name, Order) ->
  MessageRec = #seto_order_cancel{order=Order},
  gen_server:call(Name, {order_cancel, MessageRec}).

subscribe(Name, Group) ->
  MessageRec = #seto_market_subscription{group=Group},
  gen_server:call(Name, {market_subscription, MessageRec}).

unsubscribe(Name, Group) ->
  MessageRec = #seto_market_unsubscription{group=Group},
  gen_server:call(Name, {market_unsubscription, MessageRec}).

market_request(Name, Group) ->
  MessageRec = #seto_market_request{group=Group},
  gen_server:call(Name, {market_request, MessageRec}).

market_quotes_request(Name, Group) ->
  MessageRec = #seto_market_quotes_request{group=Group},
  gen_server:call(Name, {market_quotes_request, MessageRec}).
  

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Cache, Opts]) ->
  {registered_name, Name} = process_info(self(), registered_name),
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),

  {Session,LastIn,LastOut} = Cache:session_state(Name),
  LoginMessage = 
    {login, #seto_login{
        username=proplists:get_value(username, Opts),
        password=proplists:get_value(password, Opts),
        session=Session
      }},
  case proplists:get_value(callback, Opts) of
    undefined ->
      {stop, no_callback};
    Callback ->
      %{ok, #s{}}
      send_call(LoginMessage, #s{
          session=Session,
          name=Name,
          sock=Sock,
          in=LastIn+1, out=LastOut+1,
          cache=Cache,
          callback=Callback
        })
  end.

handle_call(seq, _From, #s{in=In, out=Out} = State) ->
  {reply, {In,Out}, State};

handle_call(Message, _From, State0) ->
  {Reply, State} = send_call(Message, State0),
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({heartbeat_timeout, Seq}, #s{out=Seq} = State0) ->
  {_, State} = send_call(heartbeat, State0#s{heartbeat_ref=undefined}),
  io:format("sending heartbeat~n",[]),
  {noreply, State};
handle_info({heartbeat_timeout, _}, State) ->
  {noreply, State};

handle_info({tcp, Sock, Data}, #s{buf=Buf} = State) ->
  inet:setopts(Sock, [{active,once}]),
  Buf1 = eto_frame:buf_append(Buf, Data),
  {NewBuf, Payloads} = deframe_all(Buf1, []),
  NewState =
    lists:foldl(
      fun(PayloadData, AccState) ->
        handle_payload(seto_piqi:parse_payload(PayloadData), AccState)
      end,
      State, Payloads),
  {noreply, NewState#s{buf=NewBuf}};

handle_info({tcp_closed, _Sock}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
  {stop, {tcp_error, Reason}, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_payload({sequenced, Sequenced}, State) ->
  handle_sequenced(Sequenced, State);
handle_payload(Transient, #s{callback=Callback, session=Session} = State) ->
  ok = Callback(Transient, Session),
  State.

handle_sequenced(#seto_sequenced{seq=Seq, message={replay, _} = Message},
                 #s{in=InSeq} = State) when Seq > InSeq ->
  {_, State0} = send_call({replay, #seto_replay{seq=InSeq}}, State),
  State1 = handle_message(Message, State0),
  State1;
handle_sequenced(#seto_sequenced{seq=Seq, message=Message}, #s{in=InSeq} = State0) when Seq > InSeq ->
  {_, State} = send_call({replay, #seto_replay{seq=InSeq}}, login_message(Message,State0)),
  State;
handle_sequenced(#seto_sequenced{seq=Seq, message=Message} = Payload,
                 #s{in=Seq, name=Name, session=Session, callback=Callback, cache=Cache} = State) ->
  State0 = handle_message(Message, State#s{in=Seq+1}),
  Cache:log_in(Name, Seq),
  ok = Callback(Payload, Session),
  State0;
handle_sequenced(_, State) ->
  State.

handle_message({replay, #seto_replay{seq=Seq}}, #s{name=Name, sock=Sock, cache=Cache} = State) ->
  Cache:map_from(Name, Seq,
    fun(Payload) ->
      sock_send(Sock, replay_payload(Payload))
    end),
  State;

handle_message({login_response, _} = Message, State) ->
  login_message(Message, State);

handle_message(_Message, State) ->
  State.

login_message({login_response, #seto_login_response{session=Session, reset=Reset}}, State) ->
  #s{name=Name, cache=Cache} = State,
  Cache:takeover_session(Name, Session),
  State#s{session=Session, out=Reset};
login_message(_, State) ->
  State.

send_call(Message, #s{name=Name, out=Seq, sock=Sock, cache=Cache, heartbeat_ref=Ref} = State) ->
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
