-module(hot_proxy_web_ws).

%% Cowboy callback
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).


%% API exports
-export([send/3, send/4]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init(Req, Opts) when is_map(Opts)->
	hot_proxy_event:subscribe(checkin),
	hot_proxy_event:subscribe(checkout),
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, JSON} = Data, Req, State) ->
	Message = jsx:decode(JSON, [return_maps]),
	case handle_client_task(Message, State) of
		{reply, Data, NewState} -> {reply, {text, jsx:encode(Data)}, Req, NewState};
		{ok, NewState} -> {ok, Req, NewState}
	end;
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({send, Message}, Req, State) ->
	{reply, {text, Message}, Req, State};
websocket_info(Message, Req, State) ->
	send(self(), info, erlang:iolist_to_binary(io_lib:format("~p", [Message]))),
	{ok, Req, State}.


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

send(Handler, Type, Message) ->
	send(Handler, Type, Message, #{}).

send(Handler, Type, Message, Base) when is_map(Base) ->
	Handler ! {send, jsx:encode(Base#{type => Type, content => Message })},
	ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_client_task(_Message, State) -> {ok, State}.
