-module(hot_proxy_config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
	init_tables/0,
	get_domain_servers/1,
	insert_host/2,
	insert_domain/3
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
		set,
		public,
		named_table
	]),
	ets:new(hot_proxy_config_lookup, [
		bag,
		public,
		named_table,
		{read_concurrency, true}
	]),
	%% hardcoded values, we don't care about the domain for the time being
	insert_domain(<<"hot-proxy.com">>, [<<N>> || N <- lists:seq($0,$9)], [{{127,0,0,1}, Port, 5, 1} || Port <- lists:seq(8081, 8083)]),
	ok.

get_domain_servers(Domain) ->
	{ok, {Domain, [
		RoutingData || {_Domain, _RoutingGroup, RoutingData} <- ets:lookup(hot_proxy_config_lookup, Domain)
	]}}.

insert_domain(Domain, Aliases, HostAddrs) when is_list(Aliases), is_list(HostAddrs) ->
	HostSpecs = [ {{{IP, Port}, TTL}, Weight} || {IP, Port, TTL, Weight} <- HostAddrs],
	Primary   = [ {Domain, Domain, Spec} || Spec <- HostSpecs],
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Spec <- HostSpecs, Alias <- Aliases],
	true = ets:insert(?MODULE, {Domain, Aliases, HostAddrs}),
	true = ets:insert(hot_proxy_config_lookup, Primary ++ Secondary),
	ok.

insert_host(Domain, {IP, Port, TTL, Weight}) ->
	Spec = {{{IP, Port}, TTL}, Weight},
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Alias <- Aliases],
	true = ets:insert(?MODULE, {Domain, Aliases, [Spec |HostAddrs]}),
	true = ets:insert(hot_proxy_config_lookup, [{Domain, Domain, Spec} |Secondary]),
	ok.

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
