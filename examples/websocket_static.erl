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
		{ws_autoexit, false},
		{ws_loop, fun(Ws) -> handle_websocket(Ws,self()) end}
	]).

% stop misultin
stop() ->
	misultin:stop().

% callback function called on incoming http request, if not a static e.g. http://localhost:8000/foobar
handle_http(Req) ->
	Req:ok("Not a static file request.").

% callback on received websockets data
handle_websocket(Ws,Pid) ->
	receive
		{browser, Data} ->
			io:format("Websocket ~p data: ~p~n", [Ws,Data]),
			{ok,Tokens,_} = erl_scan:string(Data),
			{ok,Term} = erl_parse:parse_term(Tokens),
			io:format("Parsed term: ~p~n", [Term]), 
			Pid1 = case Term of
				{make,Module} -> Module:make(Ws);
				Term -> Pid ! Term, Pid
			end,
			handle_websocket(Ws,Pid1);
		closed ->
			% IMPORTANT: since we specified the {ws_autoexit, false} option, we need to manually ensure that this process exits
			% [otherwise it will become a zombie]
			io:format("The WebSocket was CLOSED!~n"),
			Pid ! {quit};
		Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
			handle_websocket(Ws,Pid)
	end.
	
