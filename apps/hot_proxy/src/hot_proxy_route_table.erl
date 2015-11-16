-module(hot_proxy_route_table).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
	init_tables/0,
	check_cache/1,
	update_cache/3
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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	erlang:send_after(timer:seconds(10), self(), flush_cache),
	{ok, #{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(flush_cache, State) ->
	ets:select_delete(?MODULE, [{{'_', {'_', '$2'}}, [{'>', {const, erlang:timestamp()}, '$2'}],[true]}]),
	erlang:send_after(timer:seconds(10), self(), flush_cache),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

