% ==========================================================================================================
% MISULTIN - Socket
%
% >-|-|-(°>
% 
% Copyright (C) 2009, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
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
-vsn('0.3.2').

% API
-export([start_link/5]).

% callbacks
-export([listener/5]).

% internale
-export([socket_loop/2]).

% macros
-define(MAX_HEADERS_COUNT, 100).

% records
-record(c, {
	sock,
	port,
	loop,
	recv_timeout,
	stream_support
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
start_link(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport) ->
	proc_lib:spawn_link(?MODULE, listener, [ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport]).

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport) ->
	case catch gen_tcp:accept(ListenSocket) of
		{ok, Sock} ->
			?LOG_DEBUG("accepted an incoming TCP connection, spawning controlling process", []),
			Pid = spawn(fun () ->
				receive
					set ->
						?LOG_DEBUG("activated controlling process", []),
						ok
				after 60000 ->
					exit({error, controlling_failed})
				end,
				% build connection record
				{ok, {Addr, Port}} = inet:peername(Sock),
				C = #c{sock = Sock, port = ListenPort, loop = Loop, recv_timeout = RecvTimeout, stream_support = StreamSupport},
				% jump to state 'request'
				?LOG_DEBUG("jump to state request", []),
				request(C, #req{peer_addr = Addr, peer_port = Port})
			end),
			% set controlling process
			gen_tcp:controlling_process(Sock, Pid),
			Pid ! set,
			% get back to accept loop
			listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport);
		_Else ->
			?LOG_ERROR("accept failed error: ~p", [_Else]),
			exit({error, accept_failed})
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% REQUEST: wait for a HTTP Request line. Transition to state headers if one is received. 
request(#c{sock = Sock, recv_timeout = RecvTimeout} = C, Req) ->
	inet:setopts(Sock, [{active, once}]),
	receive
		{http, Sock, {http_request, Method, Path, Version}} ->
			?LOG_DEBUG("received full headers of a new HTTP packet", []),
			headers(C, Req#req{vsn = Version, method = Method, uri = Path, connection = default_connection(Version)}, []);
		{http, Sock, {http_error, "\r\n"}} ->
			request(C, Req);
		{http, Sock, {http_error, "\n"}} ->
			request(C, Req);
		{http, Sock, _Other} ->
			?LOG_DEBUG("tcp error on incoming request: ~p, send bad request error back", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(400))
	after RecvTimeout ->
		?LOG_DEBUG("normal receive timeout, exit", []),
		exit(normal)
	end.

% HEADERS: collect HTTP headers. After the end of header marker transition to body state.
headers(C, Req, H) ->
	headers(C, Req, H, 0).
headers(#c{sock = Sock}, _Req, _H, ?MAX_HEADERS_COUNT) ->
	?LOG_DEBUG("too many headers sent, bad request",[]),
	send(Sock, misultin_utility:get_http_status_code(400));
headers(#c{sock = Sock, recv_timeout = RecvTimeout} = C, Req, H, HeaderCount) ->
	inet:setopts(Sock, [{active, once}]),
	receive
		{http, Sock, {http_header, _, 'Content-Length', _, Val}} ->
			headers(C, Req#req{content_length = Val}, [{'Content-Length', Val}|H], HeaderCount + 1);
		{http, Sock, {http_header, _, 'Connection', _, Val}} ->
			headers(C, Req#req{connection = keep_alive(Req#req.vsn, Val)}, [{'Connection', Val}|H], HeaderCount + 1);
		{http, Sock, {http_header, _, Header, _, Val}} ->
			headers(C, Req, [{Header, Val}|H], HeaderCount + 1);
		{http, Sock, {http_error, "\r\n"}} ->
			headers(C, Req, H, HeaderCount);
		{http, Sock, {http_error, "\n"}} ->
			headers(C, Req, H, HeaderCount);
		{http, Sock, http_eoh} ->
			body(C, Req#req{headers = lists:reverse(H)});
		{http, Sock, _Other} ->
			?LOG_DEBUG("tcp error treating headers: ~p, send bad request error back", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(400))
	after RecvTimeout ->
		?LOG_DEBUG("headers timeout, sending request timeout error", []),
		send(Sock, misultin_utility:get_http_status_code(408))
	end.

% default connection
default_connection({1,1}) -> keep_alive;
default_connection(_) -> close.

% Shall we keep the connection alive? Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
keep_alive({1,1}, "close")		-> close;
keep_alive({1,1}, "Close")		-> close;
% string:to_upper is used only as last resort.
keep_alive({1,1}, Head) ->
	case string:to_upper(Head) of
		"CLOSE" -> close;
		_		-> keep_alive
	end;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, Head) ->
	case string:to_upper(Head) of
		"KEEP-ALIVE"	-> keep_alive;
		_				-> close
	end;
keep_alive({0,9}, _)			-> close;
keep_alive(_Vsn, _KA)			-> close.

% BODY: collect the body of the HTTP request if there is one, and lookup and call the implementation callback.
% Depending on whether the request is persistent transition back to state request to await the next request or exit.
body(#c{sock = Sock, recv_timeout = RecvTimeout} = C, Req) ->
	case Req#req.method of
		'GET' ->
			Close = handle_get(C, Req),
			case Close of
				close ->
					gen_tcp:close(Sock);
				keep_alive ->
					request(C, #req{peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port})
			end;
		'POST' ->
			case catch list_to_integer(Req#req.content_length) of 
				{'EXIT', _} ->
					% TODO: provide a fallback when content length is not or wrongly specified
					?LOG_DEBUG("specified content length is not a valid integer number: ~p", [Req#req.content_length]),
					send(Sock, misultin_utility:get_http_status_code(411)),
					exit(normal);
				Len ->
					inet:setopts(Sock, [{packet, raw}, {active, false}]),
					case gen_tcp:recv(Sock, Len, RecvTimeout) of
						{ok, Bin} ->
							Close = handle_post(C, Req#req{body = Bin}),
							case Close of
								close ->
									gen_tcp:close(Sock);
								keep_alive ->
									inet:setopts(Sock, [{packet, http}]),
									request(C, #req{peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port})
							end;
						{error, timeout} ->
							?LOG_DEBUG("request timeout, sending error", []),
							send(Sock, misultin_utility:get_http_status_code(408));	
						_Other ->
							?LOG_DEBUG("tcp error treating post data: ~p, send bad request error back", [_Other]),
							send(Sock, misultin_utility:get_http_status_code(400))
					end
			end;
		_Other ->
			?LOG_DEBUG("method not implemented: ~p", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(501)),
			exit(normal)
	end.

% handle a get request
handle_get(C, #req{connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {abs_path, F}}),
			Conn;
		{absoluteURI, http, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {absoluteURI, F}}),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501)),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, misultin_utility:get_http_status_code(510)),
			close;
		_  ->
			send(C#c.sock, misultin_utility:get_http_status_code(403)),
			close
	end.

% handle a post request
handle_post(C, #req{connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, http, _Host, _, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501)),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501)),
			close;
		_  ->
			send(C#c.sock, misultin_utility:get_http_status_code(403)),
			close
	end.

% Description: Main dispatcher
call_mfa(#c{sock = Sock, loop = Loop, stream_support = StreamSupport} = C, Request) ->
	% spawn listening process for Request messages [only used to support stream requests]
	case StreamSupport of
		true ->
			SocketPid = spawn(?MODULE, socket_loop, [C, Request]);
		false ->
			SocketPid = no_stream_support_proc
	end,
	% create request
	Req = misultin_req:new(Request, SocketPid),
	% call loop
	case catch Loop(Req) of
		{'EXIT', _Reason} ->
			?LOG_ERROR("worker crash: ~p", [_Reason]),
			% kill listening socket
			catch SocketPid ! shutdown,
			% send response
			send(Sock, misultin_utility:get_http_status_code(500)),
			% force exit
			exit(normal);
		{HttpCode, Headers0, Body} ->
			% received normal response
			?LOG_DEBUG("sending response", []),
			% kill listening socket
			catch SocketPid ! shutdown,
			% flatten body [optimization since needed for content length]
			BodyBinary = convert_to_binary(Body),
			% provide response
			Headers1 = add_output_header('Content-Length', {Headers0, BodyBinary}),
			Headers = add_output_header('Connection', {Headers1, Request}),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>, BodyBinary],
			send(Sock, Resp);
		{raw, Body} ->
			send(Sock, Body);
		_ ->
			% loop exited normally, kill listening socket
			catch SocketPid ! shutdown
	end.

% Description: Ensure Body is binary.
convert_to_binary(Body) when is_list(Body) ->
	list_to_binary(lists:flatten(Body));
convert_to_binary(Body) when is_binary(Body) ->
	Body;
convert_to_binary(Body) when is_atom(Body) ->
	list_to_binary(atom_to_list(Body)).

% Description: Socket loop for stream responses
socket_loop(#c{sock = Sock} = C, Request) ->
	receive
		{stream_head, HttpCode, Headers0} ->
			?LOG_DEBUG("sending stream head", []),
			Headers = add_output_header('Connection', {Headers0, Request}),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>],
			send(Sock, Resp),
			socket_loop(C, Request);
		{stream_data, Body} ->
			?LOG_DEBUG("sending stream data", []),
			send(Sock, Body),
			socket_loop(C, Request);
		stream_close ->
			?LOG_DEBUG("closing stream", []),
			close(Sock);
		shutdown ->
			?LOG_DEBUG("shutting down socket loop", []),
			shutdown
	end.

% Description: Add necessary Content-Length Header
add_output_header('Content-Length', {Headers, Body}) ->
	case proplists:get_value('Content-Length', Headers) of
		undefined ->
			[{'Content-Length', size(Body)}|Headers];
		_ExistingContentLength ->
			Headers
	end;

% Description: Add necessary Connection Header
add_output_header('Connection', {Headers, Req}) ->
	case Req#req.connection of
		undefined ->
			% nothing to echo
			Headers;
		Connection ->
			% echo
			case proplists:get_value('Connection', Headers) of
				undefined ->
					[{'Connection', connection_str(Connection)}|Headers];
				_ExistingConnectionHeaderValue ->
					Headers
			end
	end.

% Helper to Connection string
connection_str(keep_alive) -> "Keep-Alive";
connection_str(close) -> "Close".

% Description: Encode headers
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
	[atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
	[Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
	[].
enc_header_val(Val) when is_atom(Val) ->
	atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
	integer_to_list(Val);
enc_header_val(Val) ->
	Val.

% Split the path at the ?
split_at_q_mark([$?|T], Acc) ->
	{lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
	split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
	{lists:reverse(Acc), []}.

% TCP send
send(Sock, Data) ->
	?LOG_DEBUG("sending data: ~p", [Data]),
	case gen_tcp:send(Sock, Data) of
		ok ->
			ok;
		{error, _Reason} ->
			?LOG_ERROR("worker crash: ~p", [_Reason]),
			exit(normal)
	end.

% TCP close
close(Sock) ->
	?LOG_DEBUG("closing socket", []),
	case gen_tcp:close(Sock) of
		ok ->
			ok;
		{error, _Reason} ->
			?LOG_WARNING("could not close socket: ~p", [_Reason]),
			exit(normal)
	end.
	
% ============================ /\ INTERNAL FUNCTIONS =======================================================
