-module(smk_memory_message_cache).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(MAX_QUEUE, 20).

-record(s, {
    cache :: gb_tree()
  }).

-include("seto_piqi.hrl").
% uncomment this line in your own application
%-include_lib("smk_erlang_sdk/include/seto_piqi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% send payloads
-export([log_out/2, log_in/2, map_from/3, session_state/1, takeover_session/2]).
-export([connecting/1]).
%% incoming

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log_out(ClientName, Payload) ->
  gen_server:call(?SERVER, {log_out, ClientName, Payload}).

log_in(ClientName, Seq) ->
  gen_server:call(?SERVER, {log_in, ClientName, Seq}).

map_from(ClientName, Seq, Fun) ->
  gen_server:call(?SERVER, {map_from, ClientName, Seq, Fun}).

session_state(ClientName) ->
  gen_server:call(?SERVER, {session_state, ClientName}).

takeover_session(ClientName, Session) ->
  gen_server:call(?SERVER, {takeover_session, ClientName, Session}).

connecting(ClientName) ->
  gen_server:cast(?SERVER, {connecting, ClientName}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #s{cache=gb_trees:empty()}}.

handle_call({log_out, ClientName, Payload}, _From, State) ->
  {reply, ok, do_log(ClientName, Payload, State)};

handle_call({log_in, ClientName, Seq}, _From, #s{cache=Cache} = State) ->
  {Session,T,_,Out,Q} = gb_trees:get(ClientName, Cache),
  {reply, ok, State#s{cache = gb_trees:update(ClientName, {Session,T,Seq,Out,Q}, Cache)}};

handle_call({map_from, ClientName, Seq, Fun}, _From, #s{cache=Cache} = State) ->
  {reply, do_map_from(ClientName, Seq, Fun, Cache), State};

handle_call({takeover_session, ClientName, Session}, _From, #s{cache=Cache} = State) ->
  {_,T,In,Out,Q} = gb_trees:get(ClientName, Cache),
  {reply, ok, State#s{
      cache = gb_trees:update(ClientName, {Session,T,In,Out,Q}, Cache)
    }};

handle_call({session_state, ClientName}, _From, #s{cache=Cache} = State) ->
  {reply,
    case gb_trees:lookup(ClientName, Cache) of
      none ->
        {undefined,undefined,0,0};
      {value, {undefined,T,_,_,_}} ->
        {undefined,T,0,0};
      {value, {Session,T,In,Out,_}} ->
        {Session,T,In,Out}
    end,
    State};

handle_call(_, _, State) ->
  {noreply, State}.

handle_cast({connecting, ClientName}, #s{cache=Cache} = State) ->
  {noreply, State#s{
      cache =
        case gb_trees:lookup(ClientName, Cache) of
          none ->
            gb_trees:enter(ClientName, {undefined,now(),0,0,queue:new()}, Cache);
          {value, {undefined,_T,In,Out,Q}} ->
            gb_trees:update(ClientName, {undefined,now(),In,Out,Q}, Cache);
          {value, {Session,_T,In,Out,Q}} ->
            gb_trees:update(ClientName, {Session,now(),In,Out,Q}, Cache)
        end
    }};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_log(ClientName, #seto_payload{eto_payload=#eto_payload{seq=Out}} = Payload, #s{cache=Cache} = State) ->
  State#s{cache =
    case gb_trees:lookup(ClientName, Cache) of
      none ->
        gb_trees:enter(ClientName, {undefined,now(),1,Out,queue:from_list([Payload])}, Cache);
      {value, {Session,T,In,_,Q}} ->
        NewT =
          case Payload of
            #seto_payload{type=login} -> now();
            _ -> T
          end,
        gb_trees:update(ClientName,
          {Session,NewT,In,Out,queue:in(Payload,
              case queue:len(Q) of
                ?MAX_QUEUE ->
                  element(2, queue:out(Q));
                _ ->
                  Q
              end)}, Cache)
    end}.

do_map_from(ClientName, FromSeq, Fun, Cache) ->
  case gb_trees:lookup(ClientName, Cache) of
    none -> {error, empty_cache};
    {value, {_Session,_T,_In,_Out,Q}} ->
      lists:foldl(fun
        (#seto_payload{eto_payload=#eto_payload{seq=Seq}} = Payload, Seq) ->
          Fun(Payload),
          Seq+1; 
        (_, NextSeq) ->
          NextSeq 
      end, FromSeq, queue:to_list(Q)),
      ok
  end. 
