% ==========================================================================================================
% MISULTIN - Websocket Request
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
% All rights reserved.
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
-module(misultin_ws).
-vsn("0.8").

% API
-export([raw/1, get/2, send/2]).

% includes
-include("../include/misultin.hrl").

% types
-type wst() :: {misultin_ws, #ws{}, SocketPid::pid()}.


% ============================ \/ API ======================================================================

% Returns raw websocket content.
-spec raw(wst()) -> #ws{}.
raw({misultin_ws, Ws, _SocketPid}) ->
	Ws.

% Get websocket info.
-spec get(WsInfo::atom(), wst()) -> term().
get(socket, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.socket;
get(socket_mode, {misultin_ws, Ws, _SocketPid}) ->
	Ws#req.socket_mode;
get(peer_addr, {misultin_ws, #ws{headers = Headers} = Ws, _SocketPid}) ->
	Host = case misultin_utility:get_key_value("X-Real-Ip", Headers) of
		undefined ->
			case misultin_utility:get_key_value("X-Forwarded-For", Headers) of
				undefined -> undefined;
				Hosts0 -> string:strip(lists:nth(1, string:tokens(Hosts0, ",")))
			end;
		Host0 -> Host0
	end,
	case Host of
		undefined ->
			Ws#ws.peer_addr;
		_ -> 
			case inet_parse:address(Host) of
				{error, _Reason} ->
					Ws#ws.peer_addr;
				{ok, IpTuple} ->
					IpTuple
			end
	end;
get(peer_port, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.peer_port;
get(peer_cert, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.peer_cert;
get(vsn, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.vsn;
get(origin, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.origin;
get(host, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.host;
get(path, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.path;
get(headers, {misultin_ws, Ws, _SocketPid}) ->
	Ws#ws.headers.

% send data
-spec send(Data::list() | binary() | iolist(), wst()) -> term().
send(Data, {misultin_ws, _Ws, SocketPid}) ->
	SocketPid ! {send, Data}.
		
% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
