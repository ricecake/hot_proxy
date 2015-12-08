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

subscribe(Topics) when is_list(Topics)->
	gen_server:call(?MODULE, {subscribe, Topics});
subscribe(Topic) -> subscribe([Topic]).

send(Topic, Message) ->
	gen_server:cast(?MODULE, {send, self(), Topic, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, #{ subscribers => [] }}.

handle_call({subscribe, Topics}, {From, _}, #{ subscribers := Subs } = State) ->
	NewSubs = case lists:keyfind(From, 1, Subs) of
		{From, _MonRef} -> Subs;
		false          ->
			MonRef = monitor(process, From),
			[{From, MonRef} |Subs]
	end,
	true = ets:insert(?MODULE, [{Topic, From} || Topic <- Topics]),
	{reply, ok, State#{ subscribers :=  NewSubs }};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({send, From, Topic, Message}, State) ->
	[send_event(Message, From, Rec)|| Rec <- ets:lookup(?MODULE, Topic)],
	{noreply, State};
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

send_event(Message, From, {Topic, Subscriber, Fun}) ->
	Fun(Subscriber, From, {Topic, Message}),
	ok;
send_event(Message, From, {Topic, Subscriber}) ->
	Subscriber ! {hot_proxy_event, From, {Topic, Message}},
	ok.
