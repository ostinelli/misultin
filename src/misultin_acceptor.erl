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
-vsn("0.9-dev").

% API
-export([start_link/6]).

% internal
-export([init/6]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the acceptor.
-spec start_link(
	MainSupRef::pid(),
	ListenSocket::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> {ok, Pid::pid()}.
start_link(MainSupRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [MainSupRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts]),
	{ok, Pid}.

% init
-spec init(
	MainSupRef::pid(),
	ListenSocket::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> {error, Reason::term()}.
init(MainSupRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting new acceptor with pid ~p", [self()]),
	% get pid of misultin server
	Childrens = supervisor:which_children(MainSupRef),
	case lists:keyfind(server, 1, Childrens) of
		{server, ServerRef, _, _} ->
			?LOG_DEBUG("got misultin server pid: ~p", [ServerRef]),
			% get rfc table ref
			TableDateRef = misultin_server:get_table_date_ref(ServerRef),
			?LOG_DEBUG("got misultin table date reference: ~p", [TableDateRef]),
			% get pid of sessions server
			case lists:keyfind(sessions, 1, Childrens) of
				{sessions, SessionsRef, _, _} ->
					?LOG_DEBUG("got misultin sessions pid: ~p", [SessionsRef]),
					acceptor(ServerRef, SessionsRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
				_ ->
					{error, could_not_get_sessionsref}
			end;
		_ ->
			{error, could_not_get_serverref}
	end.

% Starts the socket.
-spec acceptor(
	ServerRef::pid(),
	SessionsRef::pid(),
	TableDateRef::ets:tid(),
	ListenSocket::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> [].
acceptor(ServerRef, SessionsRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	case catch misultin_socket:accept(ListenSocket, SocketMode) of
		{ok, Sock} when SocketMode =:= http ->
			?LOG_DEBUG("received a new http request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				activate_controller_process(ServerRef, SessionsRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts)
			end),
			% set controlling process
			case misultin_socket:controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					misultin_socket:close(Sock, SocketMode)
			end,					
			% get back to accept loop
			acceptor(ServerRef, SessionsRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{ok, Sock} ->
			?LOG_DEBUG("received a new https request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				case ssl:ssl_accept(Sock, 60000) of
					ok ->
						activate_controller_process(ServerRef, SessionsRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts);
					{ok, NewSock} ->
						activate_controller_process(ServerRef, SessionsRef, TableDateRef, NewSock, ListenPort, RecvTimeout, SocketMode, CustomOpts);
					{error, _Reason} ->
						% could not negotiate a SSL transaction, leave process
						?LOG_WARNING("could not negotiate a SSL transaction: ~p", [_Reason]),
						misultin_socket:close(Sock, SocketMode)
				end
			end),
			% set controlling process
			case misultin_socket:controlling_process(Sock, Pid, SocketMode) of
				ok ->
					Pid ! set;
				{error, _Reason} ->
					?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
					misultin_socket:close(Sock, SocketMode)
			end,
			% get back to accept loop
			acceptor(ServerRef, SessionsRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{error, _Error} ->
			?LOG_WARNING("accept failed with error: ~p", [_Error]),
			% get back to accept loop
			acceptor(ServerRef, SessionsRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{'EXIT', Error} ->
			?LOG_ERROR("accept exited with error: ~p, quitting process", [Error]),
			exit({error, {accept_failed, Error}})
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% activate the controller pid
-spec activate_controller_process(
	ServerRef::pid(),
	SessionsRef::pid(),
	TableDateRef::ets:tid(),
	Sock::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> ok.
activate_controller_process(ServerRef, SessionsRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	receive
		set ->
			?LOG_DEBUG("activated controlling process ~p", [self()]),
			open_connections_switch(ServerRef, SessionsRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts)
	after 60000 ->
		?LOG_ERROR("timeout waiting for set in controlling process, closing socket", []),
		misultin_socket:close(Sock, SocketMode)
	end.

% manage open connection
-spec open_connections_switch(
	ServerRef::pid(),
	SessionsRef::pid(),
	TableDateRef::ets:tid(),
	Sock::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> ok.
open_connections_switch(ServerRef, SessionsRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	case misultin_server:http_pid_ref_add(ServerRef, self()) of
		ok ->
			% get peer address and port
			case CustomOpts#custom_opts.proxy_protocol of
				false -> {PeerAddr, PeerPort} = misultin_socket:peername(Sock, SocketMode);
				true  -> {PeerAddr, PeerPort} = peername_from_proxy_line(Sock, SocketMode)
			end,
			?LOG_DEBUG("remote peer is ~p", [{PeerAddr, PeerPort}]),
			% get peer certificate, if any
			PeerCert = misultin_socket:peercert(Sock, SocketMode),
			?LOG_DEBUG("remote peer certificate is ~p", [PeerCert]),
			% jump to external callback
			?LOG_DEBUG("jump to connection logic", []),
			misultin_http:handle_data(ServerRef, SessionsRef, TableDateRef, Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts),
			ok;
		{error, _Reason} ->
			% too many open connections, send error and close [spawn to avoid locking]
			?LOG_DEBUG("~p, refusing new request", [_Reason]),
			{PeerAddr, PeerPort} = misultin_socket:peername(Sock, SocketMode),
			Msg = misultin_http:build_error_message(503, #req{peer_addr = PeerAddr, peer_port = PeerPort, connection = close}, TableDateRef, CustomOpts#custom_opts.access_log),
			misultin_socket:send(Sock, Msg, SocketMode),
			misultin_socket:close(Sock, SocketMode)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================

%% receive the first line, and extract peer address details as per http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt
peername_from_proxy_line(Sock, SocketMode) ->
	misultin_socket:setopts(Sock, [{active,once}, {packet,line}, list], SocketMode),
	receive
		{TcpOrSsl, Sock, "PROXY " ++ ProxyLine} when TcpOrSsl =:= tcp; TcpOrSsl =:= ssl ->
			case string:tokens(ProxyLine, "\r\n ") of
				[_Proto, SrcAddrStr, _DestAddr, SrcPortStr, _DestPort] ->
					{SrcPort, _}  = string:to_integer(SrcPortStr),
					{ok, SrcAddr} = inet_parse:address(SrcAddrStr),
					?LOG_DEBUG("Got peer address from proxy line: ~p",[{SrcAddr, SrcPort} ]),
					{SrcAddr, SrcPort}
			end;

		{_, Sock, FirstLine} -> 
			?LOG_DEBUG("FIRST LINE NOT 'PROXY', but 'PROXY ...' expected due to config option; line was: '~s'", [FirstLine]),
			misultin_socket:send(Sock, [
									"HTTP 502 Server Error\r\n", 
									"Connection: close\r\n\r\n",
									"<h1>PROXY line expected</h1>",
									"Misultin configured to expect PROXY line first, as per ",
									"<a href=\"http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt\">the haproxy proxy protocol spec</a>, ",
									"but first line received was:<br/><pre>\n",
									FirstLine,
									"\n</pre>"],
								 SocketMode),
			misultin_socket:close(Sock, SocketMode),
			exit(normal)

	after 1000 ->
			?LOG_DEBUG("Timeout receiving PROXY line from upstream proxy, closing", []),
			misultin_socket:send(Sock, ["HTTP 502 Server Error\r\n", 
										"Connection: close\r\n\r\n",
                                        "<h1>502 timeout on receiving proxy line from upstream</h1>"],
								 SocketMode),
			misultin_socket:close(Sock, SocketMode),
			exit(normal)
	end.
