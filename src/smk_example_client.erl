-module(smk_example_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
    buf = eto_frame:buf(),
    out :: pos_integer(),
    in :: pos_integer(),
    session :: undefined | binary()
  }).

-include("seto_piqi.hrl").
% uncomment this line in your own application
%-include_lib("smk_erlang_sdk/include/seto_piqi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/1, seq/1]).

%% send message
-export([ping/1, order/6, order_cancel/2]).
-export([subscribe/2, unsubscribe/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).
stop(Pid) ->
  gen_server:cast(Pid, stop).

seq(Pid) ->
  gen_server:call(Pid, seq).

ping(Pid) ->
  gen_server:call(Pid, ping).

order(Pid, Qty, Px, Side, Cg, C) ->
  MessageRec = #seto_order_create{quantity=Qty, price=Px, side=Side, group=Cg, contract=C},
  gen_server:call(Pid, {order_create, MessageRec}).

order_cancel(Pid, Order) ->
  MessageRec = #seto_order_cancel{order=Order},
  gen_server:call(Pid, {order_cancel, MessageRec}).

subscribe(Pid, Group) ->
  MessageRec = #seto_market_subscription{group=Group},
  gen_server:call(Pid, {market_subscription, MessageRec}).

unsubscribe(Pid, Group) ->
  MessageRec = #seto_market_unsubscription{group=Group},
  gen_server:call(Pid, {market_unsubscription, MessageRec}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),
  Out = proplists:get_value(out, Opts, 1),
  In = proplists:get_value(in, Opts, 1),
  LoginMessage = 
    {login, #seto_login{
        username=proplists:get_value(username, Opts),
        password=proplists:get_value(password, Opts),
        session=proplists:get_value(session, Opts)
      }},
  case sock_send(Sock, #seto_sequenced{seq=Out, message=LoginMessage}) of
    ok ->
      {ok, #s{sock=Sock, in=In, out=Out+1}};
    {error, Reason} ->
      {stop, Reason}
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
  io:format("TCP Socket Closed~n", []),
  {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
  {stop, {tcp_error, Reason}, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, #s{session=Sess, in=In, out=Out}) ->
  io:format("Terminated (~p) - to resume add opts {session,~p},{in,~p},{out,~p}~n", [Reason,Sess,In,Out]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_payload({sequenced, Sequenced}, State) ->
  handle_sequenced(Sequenced, State);
handle_payload({_, _} = Transient, State) ->
  io:format("Transient ~p~n", [Transient]),
  State.

handle_sequenced(#seto_sequenced{seq=Seq, message=Message}, #s{in=InSeq} = State0) when Seq > InSeq ->
  io:format("Replay from ~p to ~p~n", [InSeq, Seq]),
  {_, State} = send_call({replay, #seto_replay{seq=InSeq}}, login_message(Message,State0)),
  State;

handle_sequenced(#seto_sequenced{seq=Seq, message=Message} = _Msg, #s{in=Seq} = State) ->
  handle_message(Message, State#s{in=Seq+1}).

handle_message({replay, #seto_replay{seq=Seq}}, #s{session=Sess, sock=Sock} = State) ->
  smk_example_message_cache:map_from(Sess, Seq,
    fun(Msg) ->
      io:format("Resending ~p~n", [replay_msg(Msg)]),
      sock_send(Sock, replay_msg(Msg))
    end),
  State;

handle_message({login_response, _} = Message, State) ->
  login_message(Message, State);

handle_message(pong, State) ->
  io:format(" p", []),
  State;

handle_message(Message, State) ->
  io:format("Received ~p~n", [Message]),
  State.

login_message({login_response, #seto_login_response{session=Sess, reset=Reset}}, State) ->
  io:format("Logged in session ~p out ~p~n", [Sess, Reset]),
  State#s{session=Sess, out=Reset};
login_message(_, State) ->
  State.

send_call(Message, #s{session=Sess, out=Seq, sock=Sock} = State) ->
  Msg = #seto_sequenced{seq=Seq, message=Message},
  case sock_send(Sock, Msg) of
    ok ->
      smk_example_message_cache:log(Sess, Msg),
      {ok, State#s{out=Seq+1}};
    Error ->
      {Error, State}
  end.

sock_send(Sock, Payload) ->
  gen_tcp:send(Sock, eto_frame:frame(seto_piqi:gen_payload({sequenced, Payload}))).

replay_msg(#seto_sequenced{message={replay,_}, seq=Seq}) ->
  gapfill(Seq);
replay_msg(#seto_sequenced{message={login,_}, seq=Seq}) ->
  gapfill(Seq);
replay_msg(Msg) ->
  Msg#seto_sequenced{replay=true}.

gapfill(Seq) ->
  #seto_sequenced{message=gapfill, replay=true, seq=Seq}.

deframe_all(Buf, Acc) ->
    case eto_frame:deframe(Buf) of
        {PayloadData, Buf1} ->
            deframe_all(Buf1, [PayloadData|Acc]);
        Buf1 -> {Buf1, lists:reverse(Acc)}
    end.
