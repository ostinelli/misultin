% ==========================================================================================================
% MISULTIN - Acceptor
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
-module(misultin_acceptor).
-vsn("dev-sup-0.8").

% API
-export([start_link/7]).

% internal
-export([init/7]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Description: Starts the acceptor.
start_link(MainSupRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting new acceptor",[]),
	Pid = proc_lib:spawn_link(?MODULE, init, [MainSupRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts]),
	{ok, Pid}.
	
init(MainSupRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	% get pid of misultin server
	Childrens = supervisor:which_children(MainSupRef),
	case lists:keyfind(server, 1, Childrens) of
		{server, ServerRef, _, _} ->
			?LOG_DEBUG("got misultin server pid: ~p", [ServerRef]),
			acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts);
		_ ->
			{error, could_not_get_serverref}
	end.	

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
acceptor(ServerRef, ListenSocket, ListenPort, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	case catch misultin_socket:accept(ListenSocket, SocketMode) of
		{ok, Sock} when SocketMode =:= http ->
			?LOG_DEBUG("received a new http request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				activate_controller_process(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections)
			end),
			% set controlling process
			case misultin_socket:controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					catch misultin_socket:close(Sock, SocketMode)
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
						catch misultin_socket:close(Sock, SocketMode)
				end
			end),
			% set controlling process
			case misultin_socket:controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					catch misultin_socket:close(Sock, SocketMode)
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
		catch misultin_socket:close(Sock, SocketMode)
	end.

% manage open connection
open_connections_switch(ServerRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts, MaxConnections) ->
	case misultin_server:get_open_connections_count(ServerRef) >= MaxConnections of
		false ->
			?LOG_DEBUG("get basic info from socket ~p", [Sock]),
			% get peer address and port
			{PeerAddr, PeerPort} = misultin_socket:peername(Sock, SocketMode),
			% get peer certificate, if any
			PeerCert = misultin_socket:peercert(Sock, SocketMode),
			% jump to external callback
			?LOG_DEBUG("jump to connection logic", []),
			misultin_http:handle_data(ServerRef, Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts);
		true ->
			% too many open connections, send error and close [spawn to avoid locking]
			?LOG_WARNING("too many open connections, refusing new request",[]),
			misultin_socket:send(Sock, [misultin_utility:get_http_status_code(503), <<"Connection: Close\r\n\r\n">>], SocketMode),
			misultin_socket:close(Sock, SocketMode)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
