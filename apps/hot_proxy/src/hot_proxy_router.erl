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
	UUID = erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
	{ok, Upstream, #{ initiated => ReqTime, tried => [], request => UUID }}.

lookup_domain_name(Domain, Upstream, State) ->
	{ok, Root, Servers} = hot_proxy_config:get_domain_servers(Domain),
        EscapedRoot = pubsub:route_escape(Root),
	{ok, Servers, Upstream, State#{ root => EscapedRoot }}.

checkout_service({_Domain, []}, Upstream, State) ->
	{error, unhandled_domain, Upstream, State};
checkout_service({Domain, Servers}, Upstream, #{ initiated := ReqUUID, tried := Tried, request := UUID, root := Root } = State) ->
	{{PeerIp, _PeerPort}, Upstream2} = cowboyku_req:peer(Upstream),
	RequestKey = {Domain, PeerIp},
	{ok, {Server, _} = Prelim} = case hot_proxy_route_table:check_cache(RequestKey) of
		{hit, {CachedData, CacheTime}} when CacheTime > ReqTime -> {ok, CachedData};
		_ -> get_weighted_pick(RequestKey, Servers)
	end,
	Result = case lists:member(Server, Tried) of
		true  -> reroute_request(RequestKey, Servers, Tried);
		false -> {ok, Prelim}
	end,
	case Result of
		{error, Type} -> {error, Type, Upstream, State};
		{ok, {FinalServer, TTL} = Data} ->
			ok = hot_proxy_route_table:update_cache(RequestKey, TTL, Data),
			% Emit connection event including server client and site
			pubsub:publish(<<"route.checkout.", Root/binary>>, {UUID, FinalServer, Domain, PeerIp}),
			{service, FinalServer, Upstream2, State#{ tried := [FinalServer |Tried] }}
	end.

service_backend({IP, Port}, Upstream, State) ->
	{{IP, Port}, Upstream, State}.

checkin_service({Domain, _}, _Pick, Phase, ServState, Upstream, #{ tried := [Server |_], request := UUID, root := Root } = State) ->
	% Emit disconnection event including server, client site and phase
	{{PeerIp, _PeerPort}, _} = cowboyku_req:peer(Upstream),
	pubsub:publish(<<"route.checkin.", Root/binary>>, {UUID, Phase, Server, Domain, PeerIp, ServState}),
	{ok, Upstream, State};
checkin_service({Domain, _}, _Pick, Phase, ServState, Upstream, #{ request := UUID } = State) ->
	% Emit disconnection event including server, client site and phase
	{{PeerIp, _PeerPort}, _} = cowboyku_req:peer(Upstream),
	pubsub:publish(<<"route.termination.anomaly">>, {UUID, Phase, null, Domain, PeerIp, ServState}),
	{ok, Upstream, State}.

feature(_WhoCares, State) ->
	{disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
	{[], State}.

error_page(unavailable, _DomainGroup, Upstream, HandlerState) ->
	{{500, [], <<>>}, Upstream, HandlerState};
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

reroute_request(RequestKey, Potential, Attempted) ->
	case lists:filter(fun({{Host,_},_})-> not lists:member(Host, Attempted) end, Potential) of
		[]        -> {error, unavailable};
		Remaining -> get_weighted_pick(RequestKey, Remaining)
	end.
