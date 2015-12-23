%%%-------------------------------------------------------------------
%% @doc hot_proxy routing statistics worker supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_stats_worker_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_worker/2
]).

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

start_worker(Type, Name) ->
	supervisor:start_child(?SERVER, [#{ type => Type, name => Name }]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, { {simple_one_for_one, 0, 1}, [
		?CHILD(hot_proxy_stats_worker, worker)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
