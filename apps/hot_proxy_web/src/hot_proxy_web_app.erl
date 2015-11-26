%%%-------------------------------------------------------------------
%% @doc hot_proxy_web public API
%% @end
%%%-------------------------------------------------------------------

-module(hot_proxy_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	case hot_proxy_web_sup:start_link() of
		{ok, Pid} ->
			Dispatch = cowboy_router:compile([
				{'_', [
					{"/",             hot_proxy_web_page, index},
					{"/static/[...]", cowboy_static, {priv_dir, hot_proxy_web, "static/"}}
				]}
			]),
			{ok, _} = cowboy:start_http(http, 25, [{ip, {127,0,0,1}}, {port, 1080}],
							[{env, [{dispatch, Dispatch}]}]),
			{ok, Pid}
	end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
