%%%-------------------------------------------------------------------
%% @doc hot_proxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_sup).

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
	Modules = [hot_proxy_route_table, hot_proxy_config, hot_proxy_event],
	[ ok = Module:init_tables() || Module <- Modules],
	{ok, { {one_for_all, 0, 1}, [
		?CHILD(hot_proxy_route_table, worker),
		?CHILD(hot_proxy_config, worker),
		?CHILD(hot_proxy_event, worker)
	]} }.

%%====================================================================
%% Internal functions
%%====================================================================
