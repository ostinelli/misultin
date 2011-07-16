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
-vsn("0.8").

% API
-export([listen/3, accept/2, controlling_process/3, peername/2, peercert/2, setopts/3, recv/4, send/3, close/2]).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% socket listen
-spec listen(Port::non_neg_integer(), Options::gen_proplist(), socketmode()) -> {ok, ListenSock::socket()} | {error, Reason::term()}.
listen(Port, Options, http) -> gen_tcp:listen(Port, Options);
listen(Port, Options, ssl) -> ssl:listen(Port, Options).

% socket accept
-spec accept(ListenSocket::socket(), socketmode()) -> {ok, ListenSock::socket()} | {error, Reason::term()}.
accept(ListenSocket, http) -> gen_tcp:accept(ListenSocket);
accept(ListenSocket, ssl) ->
	try ssl:transport_accept(ListenSocket)
	catch
		error:{badmatch, {error, Reason}} ->
			{error, Reason}
	end.					

% socket controlling process
-spec controlling_process(Sock::socket(), Pid::pid(), socketmode()) -> ok | {error, Reason::term()}.
controlling_process(Sock, Pid, http) -> gen_tcp:controlling_process(Sock, Pid);
controlling_process(Sock, Pid, ssl) -> ssl:controlling_process(Sock, Pid).

% Get socket peername
-spec peername(Sock::socket(), socketmode() | function()) -> {inet:ip_address(), non_neg_integer()}.
peername(Sock, http) -> peername(Sock, fun inet:peername/1);
peername(Sock, ssl) -> peername(Sock, fun ssl:peername/1);
peername(Sock, F) ->
	case F(Sock) of
		{ok, {Addr, Port}} ->
			{Addr, Port};
		{error, _Reason} ->
			{undefined, undefined}
	end.

% Get socket certificate
-spec peercert(Sock::socket(), socketmode()) -> Cert::term() | undefined.
peercert(_Sock, http) -> undefined;
peercert(Sock, ssl) ->
	case ssl:peercert(Sock) of
		{ok, Cert} -> Cert;
		{error, _Reason} -> undefined
	end.

% socket set options
-spec setopts(Sock::socket(), Options::gen_proplist(), socketmode()) -> ok | {error, Reason::term()}.
setopts(Sock, Options, http) -> inet:setopts(Sock, Options);
setopts(Sock, Options, ssl) -> ssl:setopts(Sock, Options).

% socket receive
-spec recv(Sock::socket(), Len::non_neg_integer(), RecvTimeout::non_neg_integer(), socketmode()) -> {ok, Data::list() | binary()} | {error, Reason::term()}.
recv(Sock, Len, RecvTimeout, http) -> gen_tcp:recv(Sock, Len, RecvTimeout);
recv(Sock, Len, RecvTimeout, ssl) -> ssl:recv(Sock, Len, RecvTimeout).

% socket send
-spec send(Sock::socket(), Data::binary() | iolist() | list(), socketmode() | function()) -> ok.
send(Sock, Data, http) -> send(Sock, Data, fun gen_tcp:send/2);
send(Sock, Data, ssl) -> send(Sock, Data, fun ssl:send/2);
send(Sock, Data, F) -> 
	?LOG_DEBUG("sending data: ~p", [Data]),
	case F(Sock, Data) of
		ok ->
			ok;
		{error, _Reason} ->
			?LOG_ERROR("error sending data: ~p", [_Reason]),
			exit(kill)
	end.

% TCP close
-spec close(Sock::socket(), socketmode() | function()) -> ok.
close(Sock, http) -> close(Sock, fun gen_tcp:close/1);
close(Sock, ssl) -> close(Sock, fun ssl:close/1);
close(Sock, F) ->
	?LOG_DEBUG("closing socket", []),
	case catch F(Sock) of
		ok ->
			ok;
		_Else ->
			?LOG_WARNING("could not close socket: ~p", [_Else]),
			exit(kill)
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
