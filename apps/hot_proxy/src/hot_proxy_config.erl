-module(hot_proxy_config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
	init_tables/0,
	get_domain_servers/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init_tables() ->
	ets:new(?MODULE, [
		bag,
		public,
		named_table,
		{read_concurrency, true}
	]),
	ets:insert(?MODULE, [
		% {domain, domain group, {{{IP, Port}, ttl}, weight}}
		{<<"0.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8081}, 30}, 1}},
		{<<"0.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8082},  1}, 1}},
		{<<"1.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8081}, 30}, 1}},
		{<<"1.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8082},  1}, 1}},
		{<<"2.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8081}, 30}, 1}},
		{<<"2.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8082},  1}, 1}},
		{<<"3.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8085}, 30}, 100}},
		{<<"3.hot-proxy.com">>, <<"hot-proxy">>, {{{{127,0,0,1}, 8082},  1}, 1}}
	]),
	ok.

get_domain_servers(Domain) ->
	%% hardcoded values, we don't care about the domain for the time being
	{ok, {Domain, [
		RoutingData || {_Domain, _RoutingGroup, RoutingData} <- ets:lookup(?MODULE, Domain)
	]}}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
