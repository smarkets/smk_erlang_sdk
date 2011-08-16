-module(smk_no_cache).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(s, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([log_out/2, log_in/2, map_from/3, session_state/1, takeover_session/2]).
-export([connecting/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

log_out(_ClientName, _Payload) -> ok.

log_in(_ClientName, _Seq) -> ok.

map_from(_ClientName, _Seq, _Fun) -> ok.

session_state(_ClientName) -> {undefined,undefined,0,0}.

takeover_session(_ClientName, _Session) -> ok.

connecting(_ClientName) -> ok.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #s{}}.

handle_call(_Msg, _From, State) ->
  {stop, unhandled_call, State}.

handle_cast(_msg, State) ->
  {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
  {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
