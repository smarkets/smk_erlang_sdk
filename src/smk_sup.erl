-module(smk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type, Restart), {I, {I, start_link, Args}, Restart, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Cache =
    case application:get_env(smk, cache) of
      {ok, C} -> C;
      _ ->
        smk_memory_message_cache
    end,
  {ok, {{one_for_all, 10, 10}, [
        ?CHILD(Cache, [], worker, permanent),
        ?CHILD(smk_clients_sup, [Cache], supervisor, permanent)
      ]}}.
