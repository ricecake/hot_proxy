%%%-------------------------------------------------------------------
%% @doc hot_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	case hot_proxy_sup:start_link() of
		{ok, Pid} ->
			{ok, _} = vegur:start_http(8080, hot_proxy_router, [
				{middlewares, vegur:default_middlewares()}
			]),
			{ok, Pid}
	end.

%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
