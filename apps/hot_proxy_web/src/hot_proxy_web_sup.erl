%%%-------------------------------------------------------------------
%% @doc hot_proxy_web top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_web_sup).

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
	{ok, { {one_for_all, 0, 1}, [
		?CHILD(hot_proxy_web_monitor, worker)
	]} }.

%%====================================================================
%% Internal functions
%%====================================================================
