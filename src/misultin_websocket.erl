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
-vsn("0.9-dev").

% API
-export([check/3, connect/5]).
-export([check_headers/2, websocket_close/4, ws_loop/4, send_to_browser/2, get_wsinfo/2, session_cmd/2]).

% behaviour
-export([behaviour_info/1]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Check if the incoming request is a websocket handshake.
-spec check(WsVersions::[websocket_version()], Path::string(), Headers::http_headers()) -> false | {true, Vsn::websocket_version()}.
check(WsVersions, _Path, Headers) ->
	?LOG_DEBUG("testing for a websocket request path: ~p headers: ~p", [_Path, Headers]),	
	% checks
	check_websockets(WsVersions, Headers).

% Connect and handshake with Websocket.
-spec connect(ServerRef::pid(), SessionsRef::pid(), Req::#req{}, Ws::#ws{}, WsLoop::function()) -> true.
connect(ServerRef, SessionsRef, #req{headers = Headers} = Req, #ws{vsn = Vsn, socket = Socket, socket_mode = SocketMode, path = Path} = Ws, WsLoop) ->
	?LOG_DEBUG("building handshake response", []),
	% get data
	Origin = misultin_utility:header_get_value('Origin', Headers),
	Host = misultin_utility:header_get_value('Host', Headers),
	% build handshake
	VsnMod = get_module_name_from_vsn(Vsn),
	HandshakeServer = VsnMod:handshake(Req, Headers, {Path, Origin, Host}),
	% send handshake back
	misultin_socket:send(Socket, HandshakeServer, SocketMode),
	% add data to ws record and spawn_link controlling process
	WsT = {misultin_ws, self()},
	WsHandleLoopPid = spawn_link(fun() -> WsLoop(WsT) end),
	% trap exit
	process_flag(trap_exit, true),
	% set opts
	misultin_socket:setopts(Socket, [{packet, 0}], SocketMode),
	% add main websocket pid to misultin server reference
	misultin_server:ws_pid_ref_add(ServerRef, self()),
	% enter loop
	enter_loop(WsHandleLoopPid, SessionsRef, Ws, Req#req.headers, Origin, Host).

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
					_ ->
						% check if header has multiple parameters (for instance FF7 websockets)
						not(lists:member(Val,string:tokens(HVal,", ")))
				end		
		end
	end,
	case lists:filter(F, RequiredHeaders) of
		[] -> true;
		MissingHeaders -> MissingHeaders
	end.

% Close socket and custom handling loop dependency
-spec websocket_close(Socket::socket(), WsHandleLoopPid::pid(), SocketMode::socketmode(), WsAutoExit::boolean()) -> ok.
websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit) ->
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

% send to browser
-spec send_to_browser(WsHandleLoopPid::pid(), Data::iolist() | binary()) -> {browser, Data::list()}.
send_to_browser(WsHandleLoopPid, Data) ->
	WsHandleLoopPid ! {browser, Data}.

% get ws info from websocket process handler
-spec get_wsinfo(SocketPid::pid(), WsInfo::atom()) -> term().
get_wsinfo(SocketPid, WsInfo) ->
	misultin_utility:call(SocketPid, {wsinfo, WsInfo}).

% send a session command to the ws process handler
-spec session_cmd(SocketPid::pid(), SessionCmd::term()) -> term().
session_cmd(SocketPid, SessionCmd) ->
	misultin_utility:call(SocketPid, {session_cmd, SessionCmd}).

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% behaviour
behaviour_info(callbacks) ->
	[
		{check_websocket, 1},
		{handshake, 3},
		{handle_data, 5},
		{send_format, 2}
	];
behaviour_info(_) ->
	undefined.

% Loop to check for all available supported websocket protocols.
-spec check_websockets(VsnSupported::[websocket_version()], Headers::http_headers()) -> false | {true, Vsn::websocket_version()}.
check_websockets([], _Headers) -> false;
check_websockets([Vsn|T], Headers) ->
	?LOG_DEBUG("testing for websocket protocol ~p", [Vsn]),
	VsnMod = get_module_name_from_vsn(Vsn),
	case VsnMod:check_websocket(Headers) of
		false -> check_websockets(T, Headers);
		true -> {true, Vsn}
	end.

% enter loop
-spec enter_loop(WsHandleLoopPid::pid(), SessionsRef::pid(), Ws::#ws{}, Headers::http_headers(), Origin::string(), Host::string()) -> true.
enter_loop(WsHandleLoopPid, SessionsRef, Ws, Headers, Origin, Host) ->
	% start listening for incoming data
	ws_loop(WsHandleLoopPid, SessionsRef, Ws#ws{headers = Headers, origin = Origin, host = Host}, undefined),
	% unlink
	process_flag(trap_exit, false),
	erlang:unlink(WsHandleLoopPid).

% Main Websocket loop
-spec ws_loop(WsHandleLoopPid::pid(), SessionsRef::pid(), Ws::#ws{}, State::term()) -> ok.
ws_loop(WsHandleLoopPid, SessionsRef, #ws{vsn = Vsn, socket = Socket, socket_mode = SocketMode, ws_autoexit = WsAutoExit} = Ws, State) ->
	misultin_socket:setopts(Socket, [{active, once}], SocketMode),
	receive
		{tcp, Socket, Data} ->
			VsnMod = get_module_name_from_vsn(Vsn),
                        WsCallback = fun(D, _) -> send_to_browser(WsHandleLoopPid, D) end,
			case VsnMod:handle_data(Data, State, {Socket, SocketMode}, [], WsCallback) of
				{_, websocket_close} ->
					misultin_websocket:websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
                                {_, websocket_close, CloseData} ->
					misultin_socket:send(Socket, CloseData, SocketMode),
					misultin_websocket:websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
				{_, continue, NewState} ->
					ws_loop(WsHandleLoopPid, SessionsRef, Ws, NewState)
			end;
		{ssl, Socket, Data} ->
			VsnMod = get_module_name_from_vsn(Vsn),
                        WsCallback = fun(D, _) -> send_to_browser(WsHandleLoopPid, {browser, D}) end,
			case VsnMod:handle_data(Data, State, {Socket, SocketMode}, [], WsCallback) of
				{_, websocket_close} ->
					misultin_websocket:websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
                                {_, continue, NewState} ->
					ws_loop(WsHandleLoopPid, SessionsRef, Ws, NewState)
			end;
		{WsHandleLoopPid, {wsinfo, WsInfo}} ->
			WsResponse = case WsInfo of
				raw				-> Ws;
				socket			-> Ws#ws.socket;
				socket_mode		-> Ws#ws.socket_mode;
				peer_addr		-> Ws#ws.peer_addr;
				peer_port		-> Ws#ws.peer_port;
				peer_cert		-> Ws#ws.peer_cert;
				vsn				-> Ws#ws.vsn;
				origin			-> Ws#ws.origin;
				host			-> Ws#ws.host;
				path			-> Ws#ws.path;
				headers			-> Ws#ws.headers
			end,
			?LOG_DEBUG("received ws info for: ~p, responding with ~p", [WsInfo, WsResponse]),
			misultin_utility:respond(WsHandleLoopPid, WsResponse),
			ws_loop(WsHandleLoopPid, SessionsRef, Ws, State);
		{CallerPid, {session_cmd, SessionCmd}} ->
			?LOG_DEBUG("received a session command: ~p", [SessionCmd]),
			case misultin_utility:get_peer(Ws#ws.headers, Ws#ws.peer_addr) of
				{error, Reason} ->
					?LOG_DEBUG("error getting remote peer_addr: ~p, cannot get session", [Reason]),
					misultin_utility:respond(CallerPid, {error, {peer_addr, Reason}}),
					ws_loop(WsHandleLoopPid, SessionsRef, Ws, State);
				{ok, PeerAddr} ->
					?LOG_DEBUG("got remote peer_addr: ~p", [PeerAddr]),
					case SessionCmd of
						{session, Cookies} ->
							% websocket cannot create sessions since they don't generate set-cookies
							case misultin_sessions:session(SessionsRef, Cookies, PeerAddr, false) of
								{error, Reason} ->
									?LOG_DEBUG("error getting session: ~p", [Reason]),
									misultin_utility:respond(CallerPid, {error, Reason});
								SessionInfo ->
									?LOG_DEBUG("got session info: ~p", [SessionInfo]),
									% respond with session id
									misultin_utility:respond(CallerPid, SessionInfo),
									% loop
									ws_loop(WsHandleLoopPid, SessionsRef, Ws, State)
							end;
						{save_session_state, SessionId, SessionState} ->
							% save session state
							?LOG_DEBUG("trying to save new session state: ~p", [SessionState]),
							Response = misultin_sessions:save_session_state(SessionsRef, SessionId, SessionState, PeerAddr),
							misultin_utility:respond(CallerPid, Response),
							% loop
							ws_loop(WsHandleLoopPid, SessionsRef, Ws, State)
					end
			end;
		{tcp_closed, Socket} ->
			?LOG_DEBUG("tcp connection was closed, exit", []),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{ssl_closed, Socket} ->
			?LOG_DEBUG("ssl tcp connection was closed, exit", []),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{'EXIT', WsHandleLoopPid, Reason} ->
			case Reason of
				normal ->
					?LOG_DEBUG("linked websocket controlling loop stopped.", []);
				_ ->
					?LOG_ERROR("linked websocket controlling loop crashed with reason: ~p", [Reason])
			end,
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		{send, Data} ->
			VsnMod = get_module_name_from_vsn(Vsn),
			?LOG_DEBUG("sending data: ~p to websocket module: ~p", [Data, VsnMod]),
			misultin_socket:send(Socket, VsnMod:send_format(Data, State), SocketMode),
			ws_loop(WsHandleLoopPid, SessionsRef, Ws, State);
		shutdown ->
			?LOG_DEBUG("shutdown request received, closing websocket with pid ~p", [self()]),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode, WsAutoExit);
		_Ignored ->
			?LOG_WARNING("received unexpected message, ignoring: ~p", [_Ignored]),
			ws_loop(WsHandleLoopPid, SessionsRef, Ws, State)
	end.

% convert websocket version to module name
-spec get_module_name_from_vsn(Vsn::websocket_version()) -> atom().
get_module_name_from_vsn(Vsn) ->
	list_to_atom("misultin_websocket_" ++ atom_to_list(Vsn)).
	
% ============================ /\ INTERNAL FUNCTIONS =======================================================
