-module(hot_proxy_event).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
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

subscribe(Topic) -> ok.

send(Topic, Message) -> ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, #{ subscribers => [] }}.

handle_call({subscribe, Topic}, From, #{ subscribers => Subs } = State) ->
	MonRef = monitor(process, Socket),
	true = ets:insert(?MODULE, {Topic, From}),
	{reply, ok, State#{ subscribers => [{From, MonRef} |Subs] }};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, _Type, Subscriber, _Exit}, #{ subscribers => Subs } = State) ->
	ets:select_delete(?MODULE, [{{'_',Subscriber},[],[true]}]),
	{noreply, State#{ subscribers => Subs -- {Subscriber, Ref} }};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
