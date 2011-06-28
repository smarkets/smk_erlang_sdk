-module(smk_example_message_cache).
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
-export([log/2, map_from/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log(Sess, Msg) ->
  gen_server:call(?SERVER, {log, Sess, Msg}).

map_from(Sess, Seq, Fun) ->
  gen_server:call(?SERVER, {map_from, Sess, Seq, Fun}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #s{cache=gb_trees:empty()}}.

handle_call({log, Sess, Msg}, _From, State) ->
  {reply, ok, do_log(Sess, Msg, State)};

handle_call({map_from, Sess, Seq, Fun}, _From, #s{cache=Cache} = State) ->
  {reply, do_map_from(Sess, Seq, Fun, Cache), State};

handle_call(_, _, State) ->
  {noreply, State}.

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

do_log(Sess, Msg, #s{cache=Cache} = State) ->
  State#s{cache =
    case gb_trees:lookup(Sess, Cache) of
      none ->
        gb_trees:enter(Sess, queue:from_list([Msg]), Cache);
      {value, Q} ->
        gb_trees:update(Sess,
          queue:in(Msg,
            case queue:len(Q) of
              ?MAX_QUEUE ->
                element(2, queue:out(Q));
              _ ->
                Q
            end), Cache)
    end}.

do_map_from(Sess, FromSeq, Fun, Cache) ->
  case gb_trees:lookup(Sess, Cache) of
    none -> {error, empty_cache};
    {value, Q} ->
      lists:foldl(fun
        (#seto_message{seq=Seq} = Msg, Seq) ->
          Fun(Msg),
          Seq+1; 
        (_, NextSeq) ->
          NextSeq 
      end, FromSeq, queue:to_list(Q)),
      ok
  end. 
