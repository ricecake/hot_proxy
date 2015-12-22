-module(hot_proxy_stats_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
	init_tables/0
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
	ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	pubsub:subscribe([
			<<"config.*.host">>,
			<<"config.*.domain">>
		], fun(_Subscriber, _From, {Topic, Message}) ->
			gen_server:cast(?SERVER, {config_change, Topic, Message})
	end),
	{ok, #{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({config_change, <<"config.insert.host">>, _Message}, State) ->
	{noreply, State};
handle_cast({config_change, <<"config.remove.host">>, _Message}, State) ->
	{noreply, State};
handle_cast({config_change, <<"config.insert.domain">>, Message}, State) ->
	{noreply, State};
handle_cast({config_change, <<"config.remove.domain">>, _Message}, State) ->
	{noreply, State};
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
