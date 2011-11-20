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
-module('misultin_websocket_draft-hixie-76').
-behaviour(misultin_websocket).
-vsn("0.9-dev").

% API
-export([check_websocket/1, handshake/3, handle_data/3, send_format/2]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> true | false
% Description: Callback to check if the incoming request is a websocket request according to this protocol.
% ----------------------------------------------------------------------------------------------------------
-spec check_websocket(Headers::http_headers()) -> boolean().
check_websocket(Headers) ->
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Origin', ignore},
		{'Sec-WebSocket-Key1', ignore}, {'Sec-WebSocket-Key2', ignore}
	],
	% check for headers existance
	case misultin_websocket:check_headers(Headers, RequiredHeaders) of
		true ->
			% return
			true;
		_RemainingHeaders ->
			?LOG_DEBUG("not this protocol, remaining headers: ~p", [_RemainingHeaders]),
			false
	end.

% ----------------------------------------------------------------------------------------------------------
% Function: -> iolist() | binary()
% Description: Callback to build handshake data.
% ----------------------------------------------------------------------------------------------------------
-spec handshake(Req::#req{}, Headers::http_headers(), {Path::string(), Origin::string(), Host::string()}) -> iolist().
handshake(#req{socket = Sock, socket_mode = SocketMode, ws_force_ssl = WsForceSsl}, Headers, {Path, Origin, Host}) ->
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
		http when WsForceSsl =:= true  -> "wss"; % behind stunnel or similar, client is using ssl
		http when WsForceSsl =:= false -> "ws"
	end,
	% build challenge
	Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
	Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
	Blank1 = length([D || D <- Key1, D =:= 32]),
	Blank2 = length([D || D <- Key2, D =:= 32]),
	Part1 = erlang:list_to_integer(Ikey1) div Blank1,
	Part2 = erlang:list_to_integer(Ikey2) div Blank2,
	Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Body/binary>>,
	Challenge = erlang:md5(Ckey),
	% format
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ", WsMode, "://", lists:concat([Host, Path]), "\r\n\r\n",
		Challenge
	].

% ----------------------------------------------------------------------------------------------------------
% Function: -> websocket_close | {websocket_close, DataToSendBeforeClose::binary() | iolist()} | NewState
% Description: Callback to handle incomed data.
% ----------------------------------------------------------------------------------------------------------
-spec handle_data(Data::binary(), State::undefined | term(), {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | term().
handle_data(Data, undefined, {Socket, SocketMode, WsHandleLoopPid}) ->
	% init status
	handle_data(Data, {buffer, none}, {Socket, SocketMode, WsHandleLoopPid});
handle_data(Data, {buffer, B} = _State, {Socket, SocketMode, WsHandleLoopPid}) ->
	% read status
	i_handle_data(Data, B, {Socket, SocketMode, WsHandleLoopPid}).

% ----------------------------------------------------------------------------------------------------------
% Function: -> binary() | iolist()
% Description: Callback to format data before it is sent into the socket.
% ----------------------------------------------------------------------------------------------------------
-spec send_format(Data::iolist(), State::term()) -> iolist().
send_format(Data, _State) ->
	[0, Data, 255].

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Buffering and data handling
-spec i_handle_data(
	Data::binary(),
	Buffer::binary() | none,
	{Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | term().
i_handle_data(<<0, T/binary>>, none, {Socket, SocketMode, WsHandleLoopPid}) ->
	i_handle_data(T, <<>>, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<>>, none, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	% return status
	{buffer, none};
i_handle_data(<<255, 0>>, _L, {Socket, SocketMode, _WsHandleLoopPid}) ->
	?LOG_DEBUG("websocket close message received from client, closing websocket with pid ~p", [self()]),
	misultin_socket:send(Socket, <<255, 0>>, SocketMode),
	% return command
	websocket_close;
i_handle_data(<<255, T/binary>>, L, {Socket, SocketMode, WsHandleLoopPid}) ->
	misultin_websocket:send_to_browser(WsHandleLoopPid, binary_to_list(L)),
	i_handle_data(T, none, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<H, T/binary>>, L, {Socket, SocketMode, WsHandleLoopPid}) ->
	i_handle_data(T, <<L/binary, H>>, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<>>, L, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	{buffer, L}.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
