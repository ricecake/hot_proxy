%%%-------------------------------------------------------------------
%% @doc hot_proxy routing statistics supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_stats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	ok = hot_proxy_stats_srv:init_tables(),
	{ok, { {one_for_all, 0, 1}, [
		?CHILD(hot_proxy_stats_worker_sup, supervisor),
		?CHILD(hot_proxy_stats_srv, worker)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
