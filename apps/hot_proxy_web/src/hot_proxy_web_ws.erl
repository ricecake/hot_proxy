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
	hot_proxy_event:subscribe([<<"#">>], fun(Subscriber, _From, {Topic, Message}) ->
		handle_route_event(Subscriber, Topic, Message)
	end),
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
websocket_info({hot_proxy_event, Type = <<"route.checkin">>,  {UUID, Phase, Server, Domain, Peer, ServState}}, Req, State) ->
	{ServerIpTuple, Port} = Server,
	ServerIp = ip_to_binary(ServerIpTuple),
	PeerIp = ip_to_binary(Peer),
	Message = #{
		request => UUID,
		domain  => Domain,
		phase   => Phase,
		state   => ServState,
		client  => #{
			ip => PeerIp
		},
		server  => #{
			ip   => ServerIp,
			port => Port
		}
	},
	{reply, {text, jsx:encode(#{ type => Type, content => Message })}, Req, State};
websocket_info({hot_proxy_event, Type = <<"route.checkout">>, {UUID, Server, Domain, Peer}}, Req, State) ->
	{ServerIpTuple, Port} = Server,
	ServerIp = ip_to_binary(ServerIpTuple),
	PeerIp = ip_to_binary(Peer),
	Message = #{
		request => UUID,
		domain  => Domain,
		phase   => <<"initialize">>,
		state   => <<"normal">>,
		client  => #{
			ip => PeerIp
		},
		server  => #{
			ip   => ServerIp,
			port => Port
		}
	},
	{reply, {text, jsx:encode(#{ type => Type, content => Message })}, Req, State};
websocket_info({hot_proxy_event, Type, Event}, Req, State) ->
	{reply, {text, jsx:encode(#{ type => Type, content => erlang:iolist_to_binary(io_lib:format("~p", [Event])) })}, Req, State};
websocket_info(_Message, Req, State) ->
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

ip_to_binary(IP) when is_tuple(IP) andalso size(IP) == 4 -> erlang:list_to_binary(inet_parse:ntoa(IP)).

handle_route_event(Subscriber, Topic, Message) ->
	Subscriber ! {hot_proxy_event, Topic, Message},
	ok.
