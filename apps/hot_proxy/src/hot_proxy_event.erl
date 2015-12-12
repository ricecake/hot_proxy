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
	subscribe/2,
	send/2,
	lookup/1
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

subscribe(Topics, Callback) when is_list(Topics)->
	gen_server:call(?MODULE, {subscribe, Topics, Callback});
subscribe(Topic, Callback) -> subscribe([Topic], Callback).

send(Topic, Message) ->
	[send_event(Message, self(), Topic, Rec)|| Rec <- lookup(Topic)],
	ok.

lookup(Route) ->
	Path = binary:split(Route, <<".">>, [global]),
	[ Data || {_, Data} <- do_lookup(null, Path, [])].


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, #{ subscribers => [] }}.

handle_call({subscribe, Topics}, {From, _}, #{ subscribers := Subs } = State) ->
	NewSubs = ensure_monitor(From, Subs),
	true = ets:insert(?MODULE, lists:flatten([routify(Topic, From) || Topic <- Topics])),
	{reply, ok, State#{ subscribers :=  NewSubs }};
handle_call({subscribe, Topics, Callback}, {From, _}, #{ subscribers := Subs } = State) ->
	NewSubs = ensure_monitor(From, Subs),
	true = ets:insert(?MODULE, lists:flatten([routify(Topic, {From, Callback}) || Topic <- Topics])),
	{reply, ok, State#{ subscribers :=  NewSubs }};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({send, From, Topic, Message}, State) ->
	[send_event(Message, From, Topic, Rec)|| Rec <- lookup(Topic)],
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, _Type, Subscriber, _Exit}, #{ subscribers := Subs } = State) ->
	ets:select_delete(?MODULE, [
		{{'_',Subscriber},[],[true]},
		{{'_',{Subscriber,'_'}},[],[true]}
	]),
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

send_event(Message, From, Topic, {Subscriber, Fun}) ->
	Fun(Subscriber, From, {Topic, Message}),
	ok;
send_event(Message, From, Topic, Subscriber) ->
	Subscriber ! {hot_proxy_event, From, {Topic, Message}},
	ok.

ensure_monitor(New, Existing) when is_pid(New) ->
	case lists:keyfind(New, 1, Existing) of
		{New, _MonRef} -> Existing;
		false          ->
			MonRef = monitor(process, New),
			[{New, MonRef} |Existing]
	end.

routify(Key, Data) ->
	{ok, RevPath} = path(Key),
	[Terminal |Nodes] = lists:reverse(RevPath),
	[{Terminal, Data}|[ {Node, undefined} || Node <- Nodes]].

path(Key) when is_binary(Key) ->
	Path = binary:split(Key, <<".">>, [global]),
	{_, Nodes} = lists:foldl(fun
		(Node, {null, List}) ->
			{Node, [{null, Node} |List]};
		(Node, {Parent, List}) ->
			{<< Parent/bits, $., Node/bits >>, [{Parent, Node} |List]}
	end, {null, []}, Path),
	{ok, lists:reverse(Nodes)}.

do_lookup(_, [], Callbacks) -> Callbacks;
do_lookup(Parent, [Label], Callbacks) ->
	NewParent = if
		Parent ==  null -> Label;
		Parent =/= null -> << Parent/bits, $., Label/bits >>
	end,
	NewCallbacks = resolve_wildcards(Parent, NewParent, [], Callbacks),
	ets:lookup(?MODULE, {Parent, Label}) ++ NewCallbacks;
do_lookup(Parent, [Label |Path], Callbacks) ->
	NewParent = if
		Parent ==  null -> Label;
		Parent =/= null -> << Parent/bits, $., Label/bits >>
	end,
	NewCallbacks = resolve_wildcards(Parent, NewParent, Path, Callbacks),
	do_lookup(NewParent, Path, NewCallbacks).

subpaths(Path) ->
	{Last, SubPaths} = lists:foldl(fun
		(El, {null, Paths})->
			{[El], Paths};
		(El, {Curr, Paths})->
			{[El |Curr], [Curr |Paths]}
		end,
		{null, []},
		lists:reverse(Path)
	),
	[Last |SubPaths].

resolve_wildcards(Parent, NewParent, Path, Callbacks) ->
	StarCallbacks = case ets:lookup(?MODULE, {Parent, <<"*">>}) of
		[]     -> Callbacks;
		[_] -> do_lookup(NewParent, Path, Callbacks)
	end,
	case ets:lookup(?MODULE, {Parent, <<"#">>}) of
		[]     -> Callbacks;
		Nodes  -> Nodes ++ lists:flatten([ do_lookup(NewParent, ThisPath, StarCallbacks) || ThisPath <- subpaths(Path)])
	end.
