-module(websocket_static).
-export([start/1, stop/0]).

% start misultin http server with a static option {static, "/home/skvamme/misultin/examples/www"} 
% all files under the specified directory will be automatically served at http://localhost:8000/static/foobar.html

% start misultin http server
start(Port) ->
	misultin:start_link([
		{port, Port}, 
		{static, "/home/skvamme/misultin/examples/www"},
		{loop, fun(Req) -> handle_http(Req) end},
		{ws_loop, fun(Ws) -> handle_websocket(Ws) end}
	]).

% stop misultin
stop() ->
	misultin:stop().

% callback function called on incoming http request, if not a static e.g. http://localhost:8000/foobar
handle_http(Req) ->
	Req:ok("Not a static file request.").

% callback on received websockets data
handle_websocket(Ws) ->
	receive
		{browser, Data} ->
			io:format("Websocket ~p data: ~p~n", [Ws,Data]),
			{ok,Tokens,_} = erl_scan:string(Data),
			{ok,Term} = erl_parse:parse_term(Tokens),
			io:format("Parsed term: ~p~n", [Term]), 
			case Term of
				{make,Module} -> Module:make(Ws);
				{Pid,T} -> list_to_pid(Pid) ! T;
				Any ->io:format("Unknown Browser data: ~p~n",[Any])
			end,
			handle_websocket(Ws);
		Ignore -> io:format("Unknown message: ~p~n",[Ignore]),
			handle_websocket(Ws)
	end.
	
