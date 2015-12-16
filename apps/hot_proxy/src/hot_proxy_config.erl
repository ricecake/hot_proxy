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
        list_domains/0,
        get_domain/1,
	insert_domain/3,
	remove_domain/1,
	insert_alias/2,
	remove_alias/2,
	insert_host/2,
	remove_host/2,
	update_host/2,
	generate_test_data/0
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

generate_test_data() ->
	%% hardcoded values, we don't care about the domain for the time being
	insert_domain(<<"hot-proxy.com">>, [<<N>> || N <- lists:seq($0,$9)], [{{127,0,0,1}, Port, 5, 1} || Port <- lists:seq(8081, 8083)]).

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
	ok.

get_domain_servers(Domain) ->
	{ok, {Domain, [
		RoutingData || {_Domain, _RoutingGroup, RoutingData} <- ets:lookup(hot_proxy_config_lookup, Domain)
	]}}.

list_domains() ->
	{ok, [{Domain, {Aliases, Hosts}} || {Domain, Aliases, Hosts} <- ets:tab2list(?MODULE)]}.

get_domain(Domain) ->
	[Config] = ets:lookup(?MODULE, Domain),
	{ok, Config}.

insert_domain(Domain, Aliases, HostAddrs) when is_list(Aliases), is_list(HostAddrs) ->
	HostSpecs = [ {{{IP, Port}, TTL}, Weight} || {IP, Port, TTL, Weight} <- HostAddrs],
	Primary   = [ {Domain, Domain, Spec} || Spec <- HostSpecs],
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Spec <- HostSpecs, Alias <- Aliases],
	true = ets:insert(?MODULE, {Domain, Aliases, HostAddrs}),
	true = ets:insert(hot_proxy_config_lookup, Primary ++ Secondary),
	pubsub:publish(<<"config.insert.domain">>, {Domain, Aliases, HostAddrs}),
	ok.

remove_domain(Domain) ->
	ets:select_delete(hot_proxy_config_lookup, [{{'_',Domain,'_'},[],[true]}]),
	ets:delete(?MODULE, Domain),
	pubsub:publish(<<"config.remove.domain">>, Domain),
	ok.

insert_alias(Domain, Alias) ->
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	HostSpecs = [ {{{IP, Port}, TTL}, Weight} || {IP, Port, TTL, Weight} <- HostAddrs],
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Spec <- HostSpecs],
	true = ets:insert(?MODULE, {Domain, [Alias |Aliases], HostAddrs}),
	true = ets:insert(hot_proxy_config_lookup, Secondary),
	pubsub:publish(<<"config.insert.alias">>, {Domain, Alias}),
	ok.

remove_alias(Domain, Alias) ->
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	LookupKey = << Alias/bits, $., Domain/bits >>,
	true = ets:insert(?MODULE, {Domain, Aliases -- [Alias], HostAddrs}),
	ets:delete(hot_proxy_config_lookup, LookupKey),
	pubsub:publish(<<"config.remove.alias">>, {Domain, Alias}),
	ok.

insert_host(Domain, {IP, Port, TTL, Weight} = Host) ->
	Spec = {{{IP, Port}, TTL}, Weight},
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Alias <- Aliases],
	true = ets:insert(?MODULE, {Domain, Aliases, [Host |HostAddrs]}),
	true = ets:insert(hot_proxy_config_lookup, [{Domain, Domain, Spec} |Secondary]),
	pubsub:publish(<<"config.insert.host">>, {Domain, Host}),
	ok.

remove_host(Domain, ExHost) ->
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	NewHostAddrs = lists:filter(fun({IP, Port, _, _})-> ExHost =/= {IP, Port} end, HostAddrs),
	true = ets:insert(?MODULE, {Domain, Aliases, NewHostAddrs}),
	ets:select_delete(hot_proxy_config_lookup, [{{'_',Domain,{{ExHost,'_'},'_'}},[],[true]}]),
	pubsub:publish(<<"config.remove.host">>, {Domain, ExHost}),
	ok.

update_host(Domain, {IP, Port, TTL, Weight} = NewHost) ->
	Spec = {{{IP, Port}, TTL}, Weight},
	[{Domain, Aliases, HostAddrs}] = ets:lookup(?MODULE, Domain),
	NewHostAddrs = [
		case Host of
			{IP, Port, _, _} -> {IP, Port, TTL, Weight};
			Other -> Other
		end || Host <- HostAddrs
	],
	Secondary = [ {<< Alias/bits, $., Domain/bits >>, Domain, Spec} || Alias <- Aliases],
	true = ets:insert(?MODULE, {Domain, Aliases, NewHostAddrs}),
	true = ets:insert(hot_proxy_config_lookup, [{Domain, Domain, Spec} |Secondary]),
	ets:select_delete(hot_proxy_config_lookup, [{
		{'_',Domain,{{{IP, Port},'$1'},'$2'}},
		[{'=/=','$1',{const,TTL}},{'=/=','$2',{const,Weight}}],
		[true]
	}]),
	pubsub:publish(<<"config.update.host">>, {Domain, NewHost}),
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
