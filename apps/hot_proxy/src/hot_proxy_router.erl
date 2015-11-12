-module(hot_proxy_router).
-behaviour(vegur_interface).
-export([
	init/2,
	terminate/3,
	lookup_domain_name/3,
	checkout_service/3,
	checkin_service/6,
	service_backend/3,
	feature/2,
	additional_headers/4,
	error_page/4
]).

init(ReqTime, Upstream) ->
	{ok, Upstream, #{ initiated => ReqTime }}.

lookup_domain_name(Domain, Upstream, State) ->
	{ok, Servers} = hot_proxy_config:get_domain_servers(Domain),
	{ok, Servers, Upstream, State}.

checkout_service({_Domain, []}, Upstream, State) ->
	{error, unhandled_domain, Upstream, State};
checkout_service({Domain, Servers}, Upstream, #{ initiated := ReqTime } = State) ->
	{{PeerIp, _PeerPort}, _} = cowboyku_req:peer(Upstream),
	RequestKey = {Domain, PeerIp},
	Server = case hot_proxy_route_table:check_cache(RequestKey) of
		{hit, {CachedServer, Cached}} when Cached > ReqTime -> CachedServer;
		_ ->
			{ok, {NewServer, TTL}} = get_weighted_pick(RequestKey, Servers),
			ok = hot_proxy_route_table:update_cache(RequestKey, TTL, NewServer),
			NewServer
	end,
	{service, Server, Upstream, State}.

service_backend({IP, Port}, Upstream, State) ->
	%% extract the IP:PORT from the chosen server.
	{{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
	%% if we tracked total connections, we would decrement the counters here
	{ok, Upstream, State}.

feature(_WhoCares, State) ->
	{disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
	{[], State}.

error_page(unhandled_domain, _DomainGroup, Upstream, HandlerState) ->
	{{404, [], <<>>}, Upstream, HandlerState};
%% Vegur-returned errors that should be handled no matter what.
%% Full list in vegur_stub.erl
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Blame the caller
	{{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Blame the server
	{{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Who knows who was to blame!
	{{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
	{{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
	{{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
	{{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
	{{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
	ok.


get_weighted_pick(Key, Options) when is_list(Options) ->
	WeightedList = lists:flatten([ [{Item, N} || N <- lists:seq(1, Weight)] ||{Item, Weight} <- Options]),
	[{Item, _Shard}] = lrw:top(Key, WeightedList, 1),
	{ok, Item}.
