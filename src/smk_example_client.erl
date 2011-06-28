-module(smk_example_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(SOCK_OPTS, [
    binary,
    {active, once},
    {packet, 4},
    {recbuf, 8192},
    {send_timeout, 5000},
    {send_timeout_close, true},
    {nodelay, true}
  ]).

-record(s, {
    sock,
    out :: pos_integer(),
    in :: pos_integer()
  }).

-include("seto_piqi.hrl").
% uncomment this line in your own application
%-include_lib("smk_erlang_sdk/include/seto_piqi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% send payloads
-export([ping/1, order/6]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

ping(Pid) ->
  gen_server:call(Pid, ping).

order(Pid, Qty, Px, Side, Cg, C) ->
  PayloadRec = #seto_order_payload{req=ossp_uuid:make(v4,text), quantity=Qty, price=Px, side=Side, group=Cg, contract=C},
  gen_server:call(Pid, {order, PayloadRec}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),
  LoginPayload = 
    {login, #seto_login_payload{
        username=proplists:get_value(username, Opts),
        password=proplists:get_value(password, Opts)
      }},
  case sock_send(Sock, #seto_message{seq=1, payload=LoginPayload}) of
    ok ->
      {ok, #s{sock=Sock, in=1, out=2}};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(Payload, _From, State) ->
  send_call(Payload, State).

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Sock, Data}, State) ->
  inet:setopts(Sock, [{active,once}]),
  % todo: framing
  handle_message(Data, State);

handle_info({tcp_closed, _Sock}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
  {stop, {tcp_error, Reason}, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_message(Data, State) when is_binary(Data) ->
  handle_message(seto_piqi:parse_message(Data), State);

handle_message(#seto_message{seq=Seq} = Msg, #s{in=Seq} = State) ->
  io:format("Received ~p~n", [Msg]),
  {noreply, State#s{in=Seq+1}}.

send_call(Payload, #s{out=Seq, sock=Sock} = State) ->
  case sock_send(Sock, #seto_message{seq=Seq, payload=Payload}) of
    ok ->
      {reply, ok, State#s{out=Seq+1}};
    Error ->
      {reply, Error, State}
  end.

sock_send(Sock, #seto_message{} = Msg) ->
  gen_tcp:send(Sock, seto_piqi:gen_message(Msg)).
