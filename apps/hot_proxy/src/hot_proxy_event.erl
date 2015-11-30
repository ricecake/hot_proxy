-module(hot_proxy_event).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
	init_tables/0,
	subscribe/1,
	send/2
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
	ok.

subscribe(Topic) ->
	gen_server:call(?MODULE, {subscribe, Topic}).

send(Topic, Message) ->
	gen_server:call(?MODULE, {send, Topic, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, #{ subscribers => [] }}.

handle_call({subscribe, Topic}, From, #{ subscribers := Subs } = State) ->
	NewSubs = case lists:keyfind(From, 1, Subs) of
		{From, _MonRef} -> Subs;
		false          ->
			MonRef = monitor(process, From),
			[{From, MonRef} |Subs]
	end,
	true = ets:insert(?MODULE, {Topic, From}),
	{reply, ok, State#{ subscribers :=  NewSubs }};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, _Type, Subscriber, _Exit}, #{ subscribers := Subs } = State) ->
	ets:select_delete(?MODULE, [{{'_',Subscriber},[],[true]}]),
	{noreply, State#{ subscribers := Subs -- [{Subscriber, Ref}] }};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
