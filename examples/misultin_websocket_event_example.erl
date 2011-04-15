% ==========================================================================================================
% MISULTIN - Example: Shows misultin Websocket With an event support.
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%    following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%    products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin_websocket_event_example).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([
		{port, Port}, {loop, fun(Req) -> handle_http(Req, Port) end},
		{ws_loop, fun(Ws) -> handle_websocket(Ws) end}, {ws_autoexit, false}
	]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req, Port) ->	
	% output
	Req:ok([{"Content-Type", "text/html"}],
	["	
	<html>
		<head>
			<script type=\"text/javascript\">
				function addStatus(text){
					var date = new Date();
					document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + date + \": \" + text + \"<br>\";				
				}
				function ready(){
					if (\"WebSocket\" in window) {
						// browser supports websockets
						var ws = new WebSocket(\"ws://localhost:", erlang:integer_to_list(Port) ,"/service\");
						ws.onopen = function() {
							// websocket is connected
							addStatus(\"websocket connected!\");
							// send hello data to server.
							ws.send(\"hello server!\");
							addStatus(\"sent message to server: 'hello server'!\");
						};
						ws.onmessage = function (evt) {
							var receivedMsg = evt.data;
							addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
						};
						ws.onclose = function() {
							// websocket was closed
							addStatus(\"websocket was closed\");
						};
					} else {
						// browser does not support websockets
						addStatus(\"sorry, your browser does not support websockets.\");
					}
				}
			</script>
		</head>
		<body onload=\"ready();\">
			<div id=\"status\"></div>
		</body>
	</html>"]).

% callback on received websockets data
handle_websocket(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(["received '", Data, "'"]),
			handle_websocket(Ws);
		closed ->
			% IMPORTANT: since we specified the {ws_autoexit, false} option, we need to manually ensure that this process exits
			% [otherwise it will become a zombie]
			io:format("The WebSocket was CLOSED!~n");
		_Ignore ->
			handle_websocket(Ws)
	after 5000 ->
		Ws:send("pushing!"),
		handle_websocket(Ws)
	end.
