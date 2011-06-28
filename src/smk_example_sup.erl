-module(smk_example_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_all, 10, 10}, [
        ?CHILD(smk_example_message_cache, worker),
        ?CHILD(smk_example_clients_sup, supervisor)
      ]}}.


