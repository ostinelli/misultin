% ==========================================================================================================
% MISULTIN - WebSocket
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
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
-vsn("0.8").

% API
-export([check/2, connect/4]).
-export([check_headers/2, websocket_close/5, ws_loop/7, send_to_browser/2]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Check if the incoming request is a websocket handshake.
-spec check(Path::string(), Headers::http_headers()) -> false | {true, Vsn::websocket_version()}.
check(_Path, Headers) ->
	?LOG_DEBUG("testing for a websocket request path: ~p headers: ~p", [_Path, Headers]),
	% set supported websocket protocols, order does matter
	VsnSupported = ['draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'],	
	% checks
	check_websockets(VsnSupported, Headers).

% Connect and handshake with Websocket.
-spec connect(ServerRef::pid(), Req::#req{}, Ws::#ws{}, WsLoop::function()) -> true.
connect(ServerRef, #req{headers = Headers} = Req, #ws{vsn = Vsn, socket = Socket, socket_mode = SocketMode, path = Path, ws_autoexit = WsAutoExit, ws_no_headers = WsNoHeaders} = Ws, WsLoop) ->
	?LOG_DEBUG("building handshake response", []),
	% get data
	Origin = misultin_utility:header_get_value('Origin', Headers),
	Host = misultin_utility:header_get_value('Host', Headers),
	% build handshake
	VsnMod = list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)),
	HandshakeServer = VsnMod:handshake(Req, Headers, {Path, Origin, Host}),
	% send handshake back
	misultin_socket:send(Socket, HandshakeServer, SocketMode),
	% add data to ws record and spawn_link controlling process
	WsT = case WsNoHeaders of
		true -> {misultin_ws, Ws#ws{headers = [], origin = Origin, host = Host}, self()};
		false -> {misultin_ws, Ws#ws{headers = Req#req.headers, origin = Origin, host = Host}, self()}
	end,
	WsHandleLoopPid = spawn_link(fun() -> WsLoop(WsT) end),
	% trap exit
	process_flag(trap_exit, true),
	% set opts
	misultin_socket:setopts(Socket, [{packet, 0}], SocketMode),
	% add main websocket pid to misultin server reference
	misultin_server:ws_pid_ref_add(ServerRef, self()),
	% start listening for incoming data
	ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, undefined),
	% unlink
	process_flag(trap_exit, false),
	erlang:unlink(WsHandleLoopPid).

% Check if headers correspond to headers requirements.
-spec check_headers(Headers::http_headers(), RequiredHeaders::http_headers()) -> true | http_headers().
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

% Close socket and custom handling loop dependency
-spec websocket_close(ServerRef::pid(), Socket::socket(), WsHandleLoopPid::pid(), SocketMode::socketmode(), WsAutoExit::boolean()) -> ok.
websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit) ->
	% remove main websocket pid from misultin server reference
	misultin_server:ws_pid_ref_remove(ServerRef, self()),
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

send_to_browser(WsHandleLoopPid, Data) ->
	WsHandleLoopPid ! {browser, Data}.
	
% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Loop to check for all available supported websocket protocols.
-spec check_websockets(VsnSupportedMod::[websocket_version()], Headers::http_headers()) -> false | {true, Vsn::websocket_version()}.
check_websockets([], _Headers) -> false;
check_websockets([Vsn|T], Headers) ->
	?LOG_DEBUG("testing for websocket protocol ~p", [Vsn]),
	VsnMod = list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)),
	case VsnMod:check_websocket(Headers) of
		false -> check_websockets(T, Headers);
		true -> {true, Vsn}
	end.

% Main Websocket loop
-spec ws_loop(
	Vsn::websocket_version(),
	ServerRef::pid(),
	Socket::socket(),
	WsHandleLoopPid::pid(),
	SocketMode::socketmode(),
	WsAutoExit::boolean(),
	State::term()) -> ok.
ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, State) ->
	misultin_socket:setopts(Socket, [{active, once}], SocketMode),
	receive
		{tcp, Socket, Data} ->
			VsnMod = list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)),
			case VsnMod:handle_data(Data, State, {Socket, SocketMode, WsHandleLoopPid}) of
				websocket_close ->
					misultin_websocket:websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
				{websocket_close, Data} ->
					misultin_socket:send(Socket, Data, SocketMode),
					misultin_websocket:websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
				NewState ->
					ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, NewState)
			end;
		{ssl, Socket, Data} ->
			VsnMod = list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)),
			case VsnMod:handle_data(Data, State, {Socket, SocketMode, WsHandleLoopPid}) of
				{command, websocket_close} ->
					misultin_websocket:websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
				NewState ->
					ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, NewState)
			end;
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
			VsnMod = list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)),
			misultin_socket:send(Socket, VsnMod:send_format(Data, State), SocketMode),
			ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, State);
		shutdown ->
			?LOG_DEBUG("shutdown request received, closing websocket with pid ~p", [self()]),
			% close websocket and custom controlling loop
			websocket_close(ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		_Ignored ->
			?LOG_WARNING("received unexpected message, ignoring: ~p", [_Ignored]),
			ws_loop(Vsn, ServerRef, Socket, WsHandleLoopPid, SocketMode, WsAutoExit, State)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
