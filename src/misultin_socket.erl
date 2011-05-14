% ==========================================================================================================
% MISULTIN - Socket
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
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
-module(misultin_socket).
-vsn("0.7.1").

% API
-export([acceptor_start_link/7]).

% callbacks
-export([acceptor/7]).

% internal
-export([listen/3, setopts/3, recv/4, send/3, close/2]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
acceptor_start_link(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting new acceptor",[]),
	proc_lib:spawn_link(?MODULE, acceptor, [ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts]).

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	case catch accept(ListenSocket, SocketMode) of
		{ok, Sock} when SocketMode =:= http ->
			?LOG_DEBUG("received a new http request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				activate_controller_process(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections)
			end),
			% set controlling process
			case controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					catch close(Sock, SocketMode)
			end,					
			% get back to accept loop
			acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts);
		{ok, Sock} ->
			?LOG_DEBUG("received a new https request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				case ssl:ssl_accept(Sock, 60000) of
					ok ->
						activate_controller_process(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections);
					{ok, NewSock} ->
						activate_controller_process(ServerRef, NewSock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections);
					{error, _Reason} ->
						% could not negotiate a SSL transaction, leave process
						?LOG_WARNING("could not negotiate a SSL transaction: ~p", [_Reason]),
						catch close(Sock, SocketMode)
				end
			end),
			% set controlling process
			case controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					catch close(Sock, SocketMode)
			end,
			% get back to accept loop
			acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts);
		{error, _Error} ->
			?LOG_WARNING("accept failed with error: ~p", [_Error]),
			% get back to accept loop
			acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts);
		{'EXIT', Error} ->
			?LOG_ERROR("accept exited with error: ~p, quitting process", [Error]),
			exit({error, {accept_failed, Error}})
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% activate the controller pid
activate_controller_process(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections) ->
	receive
		set ->
			?LOG_DEBUG("activated controlling process ~p", [self()]),
			open_connections_switch(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections)
	after 60000 ->
		?LOG_ERROR("timeout waiting for set in controlling process, closing socket", []),
		catch close(Sock, SocketMode)
	end.

% manage open connection
open_connections_switch(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections) ->
	case misultin:get_open_connections_count(ServerRef) >= MaxConnections of
		false ->
			?LOG_DEBUG("get basic info from socket ~p", [Sock]),
			% get peer address and port
			{PeerAddr, PeerPort} = peername(Sock, SocketMode),
			% get peer certificate, if any
			PeerCert = peercert(Sock, SocketMode),
			% jump to external callback
			?LOG_DEBUG("jump to connection logic", []),
			misultin_http:handle_data(ServerRef, Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts);
		true ->
			% too many open connections, send error and close [spawn to avoid locking]
			?LOG_WARNING("too many open connections, refusing new request",[]),
			send(Sock, [misultin_utility:get_http_status_code(503), <<"Connection: Close\r\n\r\n">>], SocketMode),
			close(Sock, SocketMode)
	end.

% socket listen
listen(Port, Options, http) -> gen_tcp:listen(Port, Options);
listen(Port, Options, ssl) -> ssl:listen(Port, Options).

% socket accept
accept(ListenSocket, http) -> gen_tcp:accept(ListenSocket);
accept(ListenSocket, ssl) ->
	try ssl:transport_accept(ListenSocket)
	catch
		error:{badmatch, {error, Reason}} ->
			{error, Reason}
	end.					

% socket controlling process
controlling_process(Sock, Pid, http) -> gen_tcp:controlling_process(Sock, Pid);
controlling_process(Sock, Pid, ssl) -> ssl:controlling_process(Sock, Pid).

% Function: -> {PeerAddr, PeerPort} | PeerAddr = list() | undefined | PeerPort = integer() | undefined
% Description: Get socket peername
peername(Sock, http) -> peername(Sock, fun inet:peername/1);
peername(Sock, ssl) -> peername(Sock, fun ssl:peername/1);
peername(Sock, F) ->
	case F(Sock) of
		{ok, {Addr, Port}} ->
			{Addr, Port};
		{error, _Reason} ->
			{undefined, undefined}
	end.

% Function: -> Certificate | undefined
% Description: Get socket certificate
peercert(_Sock, http) -> undefined;
peercert(Sock, ssl) ->
	case ssl:peercert(Sock) of
		{ok, Cert} -> Cert;
		{error, _Reason} -> undefined
	end.

% socket set options
setopts(Sock, Options, http) -> inet:setopts(Sock, Options);
setopts(Sock, Options, ssl) -> ssl:setopts(Sock, Options).

% socket receive
recv(Sock, Len, RecvTimeout, http) -> gen_tcp:recv(Sock, Len, RecvTimeout);
recv(Sock, Len, RecvTimeout, ssl) -> ssl:recv(Sock, Len, RecvTimeout).

% socket send
send(Sock, Data, http) -> send(Sock, Data, fun gen_tcp:send/2);
send(Sock, Data, ssl) -> send(Sock, Data, fun ssl:send/2);
send(Sock, Data, F) -> 
	?LOG_DEBUG("sending data: ~p", [Data]),
	case F(Sock, Data) of
		ok ->
			ok;
		{error, _Reason} ->
			?LOG_ERROR("error sending data: ~p", [_Reason]),
			exit(normal)
	end.

% TCP close
close(Sock, http) -> close(Sock, fun gen_tcp:close/1);
close(Sock, ssl) -> close(Sock, fun ssl:close/1);
close(Sock, F) ->
	?LOG_DEBUG("closing socket", []),
	case catch F(Sock) of
		ok ->
			ok;
		_Else ->
			?LOG_WARNING("could not close socket: ~p", [_Else]),
			exit(normal)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
