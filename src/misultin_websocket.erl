% ==========================================================================================================
% MISULTIN - WebSocket
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, Joe Armstrong.
% All rights reserved.
%
% Code portions from Joe Armstrong have been originally taken under MIT license at the address:
% <http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html>
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
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
-module(misultin_websocket).
-vsn("0.7.1-dev").

% API
-export([check/2, connect/4]).


% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {true, Origin, Host, Path} | false
% Description: Check if the incoming request is a websocket handshake.
check(_Path, Headers) ->
	?LOG_DEBUG("testing for a websocket request path: ~p headers: ~p", [_Path, Headers]),
	% set supported websocket protocols, order does matter
	VsnSupported = [{'draft-hixie', 76}, {'draft-hixie', 68}],	
	% checks
	check_websockets(VsnSupported, Headers).

% Connect and handshake with Websocket.
connect(ServerRef, Req, #ws{vsn = Vsn, socket = Socket, socket_mode = SocketMode, path = Path, headers = Headers, ws_autoexit = WsAutoExit} = Ws, WsLoop) ->
	?LOG_DEBUG("building handshake response", []),
	% get data
	Origin = misultin_utility:header_get_value('Origin', Headers),
	Host = misultin_utility:header_get_value('Host', Headers),
	% build handshake
	HandshakeServer = handshake(Vsn, Req, Headers, {Path, Origin, Host}),
	% send handshake back
	misultin_socket:send(Socket, HandshakeServer, SocketMode),
	% add data to ws record and spawn_link controlling process
	Ws0 = {misultin_ws, Ws#ws{origin = Origin, host = Host}, self()},
	WsHandleLoopPid = spawn_link(fun() -> WsLoop(Ws0) end),
	% trap exit
	process_flag(trap_exit, true),
	% set opts
	misultin_socket:setopts(Socket, [{packet, 0}], SocketMode),
	% add main websocket pid to misultin server reference
	misultin:ws_pid_ref_add(ServerRef, self()),
	% start listening for incoming data
	ws_loop(ServerRef, Socket, none, WsHandleLoopPid, SocketMode, WsAutoExit),
	% unlink
	process_flag(trap_exit, false),
	erlang:unlink(WsHandleLoopPid).
	
% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Description: Loop to check for all available supported websocket protocols.
check_websockets([], _Headers) -> false;
check_websockets([Vsn|T], Headers) ->
	case check_websocket(Vsn, Headers) of
		false -> check_websockets(T, Headers);
		{true, Vsn} -> {true, Vsn}
	end.

% Function: {true, Vsn} | false
% Description: Check if the incoming request is a websocket request.
check_websocket({'draft-hixie', 76} = Vsn, Headers) ->
	?LOG_DEBUG("testing for websocket protocol ~p", [Vsn]),
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore},
		{'Sec-WebSocket-Key1', ignore}, {'Sec-WebSocket-Key2', ignore}
	],
	% check for headers existance
	case check_headers(Headers, RequiredHeaders) of
		true ->
			% return
			{true, Vsn};
		_RemainingHeaders ->
			?LOG_DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
			false
	end;
check_websocket({'draft-hixie', 68} = Vsn, Headers) ->
	?LOG_DEBUG("testing for websocket protocol ~p", [Vsn]),
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore}
	],
	% check for headers existance
	case check_headers(Headers, RequiredHeaders) of
		true -> {true, Vsn};
		_RemainingHeaders ->
			?LOG_DEBUG("not protocol ~p, remaining headers: ~p", [Vsn, _RemainingHeaders]),
			false
	end.

% Function: true | [{RequiredTag, RequiredVal}, ..]
% Description: Check if headers correspond to headers requirements.
check_headers(Headers, RequiredHeaders) ->
	F = fun({Tag, Val}) ->
		% see if the required Tag is in the Headers
		case misultin_utility:header_get_value(Tag, Headers) of
			false -> true; % header not found, keep in list
			HVal ->
				case Val of
					ignore -> false; % ignore value -> ok, remove from list
					HVal -> false;	 % expected val -> ok, remove from list
					_ -> true		 % val is different, keep in list
				end		
		end
	end,
	case lists:filter(F, RequiredHeaders) of
		[] -> true;
		MissingHeaders -> MissingHeaders
	end.

% Function: List
% Description: Builds the server handshake response.
handshake({'draft-hixie', 76}, #req{socket = Sock, socket_mode = SocketMode}, Headers, {Path, Origin, Host}) ->
	% build data
	Key1 = misultin_utility:header_get_value('Sec-WebSocket-Key1', Headers),
	Key2 = misultin_utility:header_get_value('Sec-WebSocket-Key2', Headers),
	% handshake needs body of the request, still need to read it [TODO: default recv timeout hard set, will be exported when WS protocol is final]
	misultin_socket:setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
	Body = case misultin_socket:recv(Sock, 8, 30*1000, SocketMode) of
		{ok, Bin} -> Bin;
		{error, timeout} ->
			?LOG_WARNING("timeout in reading websocket body", []),
			<<>>; 
		_Other ->
			?LOG_ERROR("tcp error treating data: ~p", [_Other]),
			<<>>
	end,
	?LOG_DEBUG("got content in body of websocket request: ~p", [Body]),	
	% prepare handhsake response
	WsMode = case SocketMode of
		ssl -> "wss";
		_ -> "ws"
	end,
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ", WsMode, "://", lists:concat([Host, Path]), "\r\n\r\n",
		build_challenge({'draft-hixie', 76}, {Key1, Key2, Body})
	];
handshake({'draft-hixie', 68}, #req{socket_mode = SocketMode} = _Req, _Headers, {Path, Origin, Host}) ->
	% prepare handhsake response
	WsMode = case SocketMode of
		ssl -> "wss";
		_ -> "ws"
	end,
	["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"WebSocket-Origin: ", Origin , "\r\n",
		"WebSocket-Location: ", WsMode, "://", lists:concat([Host, Path]), "\r\n\r\n"
	].

% Function: List
% Description: Builds the challenge for a handshake response.
% Code portions from Sergio Veiga <http://sergioveiga.com/index.php/2010/06/17/websocket-handshake-76-in-erlang/>
build_challenge({'draft-hixie', 76}, {Key1, Key2, Key3}) ->
	Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
	Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
	Blank1 = length([D || D <- Key1, D =:= 32]),
	Blank2 = length([D || D <- Key2, D =:= 32]),
	Part1 = erlang:list_to_integer(Ikey1) div Blank1,
	Part2 = erlang:list_to_integer(Ikey2) div Blank2,
	Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Key3/binary>>,
	erlang:md5(Ckey).

% Main Websocket loop
ws_loop(ServerRef, Socket, Buffer, WsHandleLoopPid, SocketMode, WsAutoExit) ->
	misultin_socket:setopts(Socket, [{active, once}], SocketMode),
	receive
		{tcp, Socket, Data} ->
			handle_data(Buffer, binary_to_list(Data), Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
		{ssl, Socket, Data} ->
			handle_data(Buffer, binary_to_list(Data), Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
		{tcp_closed, Socket} ->
			?LOG_DEBUG("tcp connection was closed, exit", []),
			% close websocket and custom controlling loop
			websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{ssl_closed, Socket} ->
			?LOG_DEBUG("ssl tcp connection was closed, exit", []),
			% close websocket and custom controlling loop
			websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{'EXIT', WsHandleLoopPid, Reason} ->
			case Reason of
				normal ->
					?LOG_DEBUG("linked websocket controlling loop stopped.", []);
				_ ->
					?LOG_ERROR("linked websocket controlling loop crashed with reason: ~p", [Reason])
			end,
			% close websocket and custom controlling loop
			websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{send, Data} ->
			?LOG_DEBUG("sending data to websocket: ~p", [Data]),
			misultin_socket:send(Socket, [0, Data, 255], SocketMode),
			ws_loop(ServerRef, Socket, Buffer, WsHandleLoopPid, SocketMode, WsAutoExit);
		shutdown ->
			?LOG_DEBUG("shutdown request received, closing websocket with pid ~p", [self()]),
			% close websocket and custom controlling loop
			websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		_Ignored ->
			?LOG_WARNING("received unexpected message, ignoring: ~p", [_Ignored]),
			ws_loop(ServerRef, Socket, Buffer, WsHandleLoopPid, SocketMode, WsAutoExit)
	end.

% Buffering and data handling
handle_data(none, [0|T], Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	handle_data([], T, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
handle_data(none, [], Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	ws_loop(ServerRef, Socket, none, WsHandleLoopPid, SocketMode, WsAutoExit);
handle_data(none, [255|T], Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit),
	handle_data(none, T, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
handle_data(L, [255|T], Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	WsHandleLoopPid ! {browser, lists:reverse(L)},
	handle_data(none, T, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
handle_data(L, [H|T], Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	handle_data([H|L], T, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef);
handle_data([], L, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, ServerRef) ->
	ws_loop(ServerRef, Socket, L, WsHandleLoopPid, SocketMode, WsAutoExit).

% Close socket and custom handling loop dependency
websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit) ->
	% remove main websocket pid from misultin server reference
	misultin:ws_pid_ref_remove(ServerRef, self()),
	case WsAutoExit of
		true ->
			% kill custom handling loop process
			exit(WsHandleLoopPid, kill);
		false ->
			% the killing of the custom handling loop process is handled in the loop itself -> send event
			WsHandleLoopPid ! closed
	end,
	% close main socket
	misultin_socket:close(Socket, SocketMode).

% ============================ /\ INTERNAL FUNCTIONS =======================================================
