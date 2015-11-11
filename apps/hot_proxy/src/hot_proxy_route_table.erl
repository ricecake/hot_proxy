-module(hot_proxy_route_table).

%% API
-export([
	init/0,
	check_cache/1,
	update_cache/3
]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
	ets:new(?MODULE, [
		set,
		public,
		named_table,
		{write_concurrency, true}
	]),
	ok.

check_cache(Key) ->
	case ets:lookup(?MODULE, Key) of
		[] -> miss;
		[{Key, Data}] -> {hit, Data}
	end.

update_cache(Key, TTL, Data) ->
	{Mega, Sec, Micro} = erlang:timestamp(),
	true = ets:insert(?MODULE, {Key, {Data, {Mega, Sec+TTL, Micro}}}),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================

