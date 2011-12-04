% ==========================================================================================================
% MISULTIN - Websocket Sessions Example.
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
-module(misultin_websocket_sessions_example).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req, Port) end}, {ws_loop, fun(Ws) -> handle_websocket(Ws) end}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req, Port) ->
	% get session info
	{SessionId, _SessionState} = Req:session(),
	% save user's peer_addr and a resetted counter as session's state. a more complex state can easily be saved here, such as proplist()
	Req:save_session_state(SessionId, {Req:get(peer_addr), 1}),
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
					var ws;
					if (\"WebSocket\" in window) {
						ws = new WebSocket(\"ws://localhost:", erlang:integer_to_list(Port) ,"/service\");
					} else if (\"MozWebSocket\" in window) {
						ws = new MozWebSocket(\"ws://localhost:", erlang:integer_to_list(Port) ,"/service\");
					}
					if (ws) {
						// browser supports websockets
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
	% get session info
	{SessionId, {UserPeerAddr, Count}} = Ws:session(),
	receive
		{browser, Data} ->
			Ws:send(["received '", Data, "'"]),
			handle_websocket(Ws);
		_Ignore ->
			handle_websocket(Ws)
	after 5000 ->
		% increase pushed counter and save new sessin state
		Ws:save_session_state(SessionId, {UserPeerAddr, Count + 1}),
		% build push message
		Pushmessage = lists:flatten(io_lib:format("pushed ~p time(s) for user with session IP: ~p", [Count, UserPeerAddr])),
		Ws:send(Pushmessage),
		handle_websocket(Ws)
	end.
