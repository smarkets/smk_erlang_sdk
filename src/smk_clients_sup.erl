-module(smk_clients_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, start_client/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Cache) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Cache]).

start_client(Name, Opts) ->
  supervisor:start_child(?SERVER, [Name, Opts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Cache]) ->
  {ok, {{simple_one_for_one, 10, 10},
      [{undefined, {smk_client, start_link, [Cache]},
          temporary, brutal_kill, worker, [smk_client]}]}}.


