-module(smk_example_clients_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Opts) ->
  supervisor:start_child(?SERVER, [Opts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{simple_one_for_one, 10, 10},
      [{undefined, {smk_example_client, start_link, []},
          temporary, brutal_kill, worker, [smk_example_client]}]}}.


