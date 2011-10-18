-module(smk_clients_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, start_client/1, start_client/2, terminate_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Cache) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Cache]).

start_client(Opts) ->
  supervisor:start_child(?SERVER, [Opts]).

start_client(Name, Opts) ->
  supervisor:start_child(?SERVER, [Name, Opts]).

terminate_client(Name) ->
  supervisor:terminate_child(?SERVER, Name).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Cache]) ->
  Restart =
    case application:get_env(smk, restart_strategy) of
      {ok, R} -> R;
      _ -> transient
    end,
  {ok, {{simple_one_for_one, 10, 10},
      [{undefined, {smk_client, start_link, [Cache]},
          Restart, brutal_kill, worker, [smk_client]}]}}.
