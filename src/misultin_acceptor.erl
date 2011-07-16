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
-vsn("0.8").

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
	CustomOpts::misultin_option_server()) -> {ok, Pid::pid()}.
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
	CustomOpts::misultin_option_server()) -> {error, Reason::term()}.
init(MainSupRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting new acceptor with pid ~p", [self()]),
	% get pid of misultin server
	Childrens = supervisor:which_children(MainSupRef),
	case lists:keyfind(server, 1, Childrens) of
		{server, ServerRef, _, _} ->
			?LOG_DEBUG("got misultin server pid: ~p", [ServerRef]),
			% get rfc table ref
			TableDateRef = misultin_server:get_table_date_ref(ServerRef),
			acceptor(ServerRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		_ ->
			{error, could_not_get_serverref}
	end.	

% Starts the socket.
-spec acceptor(
	ServerRef::pid(),
	TableDateRef::ets:tid(),
	ListenSocket::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::misultin_option_server()) -> [].
acceptor(ServerRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	case catch misultin_socket:accept(ListenSocket, SocketMode) of
		{ok, Sock} when SocketMode =:= http ->
			?LOG_DEBUG("received a new http request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				activate_controller_process(ServerRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts)
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
			acceptor(ServerRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{ok, Sock} ->
			?LOG_DEBUG("received a new https request, spawning a controlling process",[]),
			Pid = spawn(fun() ->
				case ssl:ssl_accept(Sock, 60000) of
					ok ->
						activate_controller_process(ServerRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts);
					{ok, NewSock} ->
						activate_controller_process(ServerRef, TableDateRef, NewSock, ListenPort, RecvTimeout, SocketMode, CustomOpts);
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
			acceptor(ServerRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{error, _Error} ->
			?LOG_WARNING("accept failed with error: ~p", [_Error]),
			% get back to accept loop
			acceptor(ServerRef, TableDateRef, ListenSocket, ListenPort, RecvTimeout, SocketMode, CustomOpts);
		{'EXIT', Error} ->
			?LOG_ERROR("accept exited with error: ~p, quitting process", [Error]),
			exit({error, {accept_failed, Error}})
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% activate the controller pid
-spec activate_controller_process(
	ServerRef::pid(),
	TableDateRef::ets:tid(),
	Sock::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::misultin_option_server()) -> ok.
activate_controller_process(ServerRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	receive
		set ->
			?LOG_DEBUG("activated controlling process ~p", [self()]),
			open_connections_switch(ServerRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts)
	after 60000 ->
		?LOG_ERROR("timeout waiting for set in controlling process, closing socket", []),
		misultin_socket:close(Sock, SocketMode)
	end.

% manage open connection
-spec open_connections_switch(
	ServerRef::pid(),
	TableDateRef::ets:tid(),
	Sock::socket(),
	ListenPort::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::misultin_option_server()) -> ok.
open_connections_switch(ServerRef, TableDateRef, Sock, ListenPort, RecvTimeout, SocketMode, CustomOpts) ->
	case misultin_server:http_pid_ref_add(ServerRef, self()) of
		{ok, HttpMonRef} ->
			% get peer address and port
			{PeerAddr, PeerPort} = misultin_socket:peername(Sock, SocketMode),
			?LOG_DEBUG("remote peer is ~p", [{PeerAddr, PeerPort}]),
			% get peer certificate, if any
			PeerCert = misultin_socket:peercert(Sock, SocketMode),
			?LOG_DEBUG("remote peer certificate is ~p", [PeerCert]),
			% jump to external callback
			?LOG_DEBUG("jump to connection logic", []),
			misultin_http:handle_data(ServerRef, TableDateRef, Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts),
			% remove pid reference and demonitor
			misultin_server:http_pid_ref_remove(ServerRef, self(), HttpMonRef);
		{error, _Reason} ->
			% too many open connections, send error and close [spawn to avoid locking]
			?LOG_WARNING("~p, refusing new request", [_Reason]),
			misultin_socket:send(Sock, [misultin_utility:get_http_status_code(503), <<"Connection: Close\r\n\r\n">>], SocketMode),
			misultin_socket:close(Sock, SocketMode)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
