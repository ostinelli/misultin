% ==========================================================================================================
% MISULTIN - Http(s)
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
-module(misultin_http).
-vsn("0.9").

% API
-export([handle_data/11, build_error_message/4, build_error_message/5, build_error_message_body/1, build_error_message_body/2, get_reqinfo/2, session_cmd/2, body_recv/1]).

% internal
-export([request/2]).

% macros
-define(MAX_HEADERS_COUNT, 100).
-define(SUPPORTED_ENCODINGS, ["gzip", "deflate"]).
-define(SERVER_VERSION_TAG, "misultin/0.9").

% records
-record(c, {
	server_ref			= undefined	:: undefined | pid(),
	sessions_ref		= undefined	:: undefined | pid(),
	table_date_ref		= undefined	:: undefined | ets:tid(),
	port				= undefined :: undefined | non_neg_integer(),
	recv_timeout		= undefined :: undefined | non_neg_integer(),
	post_max_size		= undefined :: undefined | non_neg_integer(),
	get_url_max_size	= undefined :: undefined | non_neg_integer(),
	compress			= false :: boolean(),
	loop				= undefined :: undefined | function(),
	autoexit			= true :: boolean(),
	ws_loop				= undefined :: undefined | function(),
	ws_autoexit			= true :: boolean(),
	ws_versions			= undefined :: [websocket_version()],
	access_log			= undefined :: undefined | function(),
	ws_force_ssl		= false :: boolean(),
	auto_recv_body		= true :: boolean(),
	static				= false :: boolean()
}).
-record(req_options, {
	comet				= false :: boolean()		% if comet =:= true, we will monitor client tcp close
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Callback from misultin_socket
-spec handle_data(
	ServerRef::pid(),
	SessionsRef::pid(),
	TableDateRef::ets:tid(),
	Sock::socket(),
	SocketMode::socketmode(),
	ListenPort::non_neg_integer(),
	PeerAddr::inet:ip_address(),
	PeerPort::non_neg_integer(),
	PeerCert::term(),
	RecvTimeout::non_neg_integer(),
	CustomOpts::#custom_opts{}) -> ok | ignored | ssl_closed | tcp_closed | true.
handle_data(ServerRef, SessionsRef, TableDateRef, Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts) ->
	% build C record
	C = #c{
		server_ref = ServerRef,
		sessions_ref = SessionsRef,
		table_date_ref = TableDateRef,
		port = ListenPort,
		recv_timeout = RecvTimeout,
		post_max_size = CustomOpts#custom_opts.post_max_size,
		get_url_max_size = CustomOpts#custom_opts.get_url_max_size,
		compress = CustomOpts#custom_opts.compress,
		loop = CustomOpts#custom_opts.loop,
		autoexit = CustomOpts#custom_opts.autoexit,
		ws_loop = CustomOpts#custom_opts.ws_loop,
		ws_autoexit = CustomOpts#custom_opts.ws_autoexit,
		ws_versions = CustomOpts#custom_opts.ws_versions,
		access_log = CustomOpts#custom_opts.access_log,
		ws_force_ssl = CustomOpts#custom_opts.ws_force_ssl,
		auto_recv_body = CustomOpts#custom_opts.auto_recv_body,
		static = CustomOpts#custom_opts.static
	},
	Req = #req{socket = Sock, socket_mode = SocketMode, peer_addr = PeerAddr, peer_port = PeerPort, peer_cert = PeerCert},
	% enter loop
	request(C, Req).

% build error message
-spec build_error_message(HttpCode::non_neg_integer(), Req::#req{}, TableDateRef::ets:tid(), AccessLogFun::function()) -> iolist().
-spec build_error_message(HttpCode::non_neg_integer(), Req::#req{}, TableDateRef::ets:tid(), AccessLogFun::function(), MessageBody::iolist() | list() | binary()) -> iolist().
build_error_message(HttpCode, Req, TableDateRef, AccessLogFun) ->
	build_error_message(HttpCode, Req, TableDateRef, AccessLogFun, "").
build_error_message(HttpCode, Req, TableDateRef, AccessLogFun, MessageBody) ->
	?LOG_DEBUG("building error message with httpcode: ~p and message body: ~p", [HttpCode, MessageBody]),
	% building body
	Body = build_error_message_body(HttpCode, MessageBody),
	% build headers
	Headers = [{'Content-Length', erlang:iolist_size(Body)}, {'Connection', connection_str(Req#req.connection)}],
	Enc_headers = enc_headers(Headers),
	% info log
	build_access_log(Req, HttpCode, 0, TableDateRef, AccessLogFun),
	% build and send response
	[misultin_utility:get_http_status_code(HttpCode, Req#req.vsn), Enc_headers, "\r\n", Body].	

% build error message message body
-spec build_error_message_body(HttpCode::non_neg_integer()) -> iolist().
-spec build_error_message_body(HttpCode::non_neg_integer(), MessageBody::iolist() | list() | binary()) -> iolist().
build_error_message_body(HttpCode) ->
	build_error_message_body(HttpCode, <<>>).
build_error_message_body(HttpCode, MessageBody) ->
	[
		"<h1>", misultin_utility:get_http_status_message(HttpCode), "</h1>\r\n",
		MessageBody,
		"<hr><i>", ?SERVER_VERSION_TAG, "</i>"
	].
	
% get request info from http process handler
-spec get_reqinfo(SocketPid::pid(), ReqInfo::atom()) -> term().
get_reqinfo(SocketPid, ReqInfo) ->
	misultin_utility:call(SocketPid, {reqinfo, ReqInfo}).

% send a session command to the http process handler
-spec session_cmd(SocketPid::pid(), SessionCmd::term()) -> term().
session_cmd(SocketPid, SessionCmd) ->
	misultin_utility:call(SocketPid, {session_cmd, SessionCmd}).

% call body_recv on the http process handler
-spec body_recv(SocketPid::pid()) -> {ok | chunk, binary()} | end_of_chunks | {error, Reason::term()}.
body_recv(SocketPid) ->
	misultin_utility:call(SocketPid, body_recv).

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% REQUEST: wait for a HTTP Request line. Transition to state headers if one is received.
-spec request(C::#c{}, Req::#req{}) -> ok | ignored | ssl_closed | tcp_closed | true.
request(#c{recv_timeout = RecvTimeout, get_url_max_size = GetUrlMaxSize} = C, #req{socket = Sock, socket_mode = SocketMode} = Req) ->
	misultin_socket:setopts(Sock, [{active, once}, {packet, http}], SocketMode),
	receive
		{SocketMode, Sock, {http_request, Method, {_, Uri} = Path, Version}} when length(Uri) > GetUrlMaxSize ->
			?LOG_WARNING("get url request uri of ~p exceed maximum length of ~p", [length(Uri), GetUrlMaxSize]),				
			misultin_socket:send(Sock, build_error_message(414, Req#req{connection = close, vsn = Version, method = Method, uri = Path}, C#c.table_date_ref, C#c.access_log), SocketMode),
			handle_keepalive(close, C, Req);		
		{SocketMode, Sock, {http_request, Method, Path, Version}} ->
			?LOG_DEBUG("received full headers of a new HTTP packet", []),
			% change packet type if in ssl mode
			case SocketMode of
				ssl -> misultin_socket:setopts(Sock, [{packet, httph}], SocketMode);
				_ -> ok
			end,
			% go to headers
			headers(C, Req#req{vsn = Version, method = Method, uri = Path, connection = default_connection(Version)}, []);
		{SocketMode, Sock, {http_error, "\r\n"}} ->
			?MODULE:request(C, Req);
		{SocketMode, Sock, {http_error, "\n"}} ->
			?MODULE:request(C, Req);
		{http, Sock, {http_error, _Other}}  ->
			?LOG_DEBUG("not the beginning of a request [maybe a ssl request while socket in http mode?]: ~p, sending bad request message and closing socket", [_Other]),
			misultin_socket:send(Sock, build_error_message(400, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
			misultin_socket:close(Sock, SocketMode);
		{tcp_closed, _Socket} ->
			?LOG_DEBUG("tcp connection was closed, exit", []),
			tcp_closed;
		{ssl_closed, _Socket} ->
			?LOG_DEBUG("ssl tcp connection was closed, exit", []),
			ssl_closed;
		shutdown ->
			?LOG_DEBUG("shutdown message received from server, exit", []),
			misultin_socket:close(Sock, SocketMode);
		_Other ->
			?LOG_WARNING("tcp error on incoming request: ~p, closing socket and exiting", [_Other]),
			misultin_socket:close(Sock, SocketMode)
	after RecvTimeout ->
		?LOG_DEBUG("normal receive timeout, closing socket and exiting", []),
		misultin_socket:close(Sock, SocketMode)
	end.

% HEADERS: collect HTTP headers. After the end of header marker transition to body state.
-spec headers(C::#c{}, Req::#req{}, H::http_headers()) -> ok | ignored | ssl_closed | tcp_closed | true.
-spec headers(C::#c{}, Req::#req{}, H::http_headers(), HCount::non_neg_integer()) -> ok | ignored | ssl_closed | tcp_closed | true.
headers(C, Req, H) ->
	headers(C, Req, H, 0).
headers(C, #req{socket = Sock, socket_mode = SocketMode} = Req, _H, ?MAX_HEADERS_COUNT) ->
	?LOG_DEBUG("too many headers sent, bad request",[]),
	misultin_socket:send(Sock, build_error_message(400, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
	handle_keepalive(close, C, Req);
headers(#c{recv_timeout = RecvTimeout, ws_loop = WsLoop} = C, #req{socket = Sock, socket_mode = SocketMode} = Req, H, HeaderCount) ->
	misultin_socket:setopts(Sock, [{active, once}], SocketMode),
	receive
		{SocketMode, Sock, {http_header, _, 'Content-Length', _, Val} = _Head} ->
			?LOG_DEBUG("received content-length header: ~p", [_Head]),
			case catch erlang:list_to_integer(Val) of
				{'EXIT', _} ->
					?LOG_DEBUG("no valid content length: ~p", [Val]),
					misultin_socket:send(Sock, build_error_message(400, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
					handle_keepalive(Req#req.connection, C, Req);
				ContentLength ->
					headers(C, Req#req{content_length = ContentLength}, [{'Content-Length', Val}|H], HeaderCount + 1)
			end;
		{SocketMode, Sock, {http_header, _, 'Connection', _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req#req{connection = keep_alive(Req#req.vsn, Val)}, [{'Connection', Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_header, _, Header, _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, [{Header, Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_error, "\r\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, {http_error, "\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, http_eoh} ->
			?LOG_DEBUG("received EOH header", []),
			Headers = lists:reverse(H),
			{_PathType, Path} = Req#req.uri,
			% check if it's a websocket request
			CheckWs = case WsLoop of
				undefined -> false;
				_Function -> misultin_websocket:check(C#c.ws_versions, Path, Headers)
			end,
			case CheckWs of
				false ->
					?LOG_DEBUG("normal http request received", []),
					% build final req with headers, uri and args, and then send to method dispatch
					case get_uri_and_args(Req#req{headers = Headers}) of
						{error, HttpErrorCode} ->
							?LOG_WARNING("error encountered when parsing uri and args: ~p", [HttpErrorCode]),
							misultin_socket:send(Sock, build_error_message(HttpErrorCode, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
							handle_keepalive(Req#req.connection, C, Req);
						Req0 ->
							method_dispatch(C, Req0)
					end;
				{true, Vsn} ->
					?LOG_DEBUG("websocket request received", []),
					misultin_websocket:connect(C#c.server_ref, C#c.sessions_ref, Req#req{headers = Headers, ws_force_ssl = C#c.ws_force_ssl}, #ws{vsn = Vsn, socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, path = Path, ws_autoexit = C#c.ws_autoexit}, WsLoop)
			end;
		{SocketMode, Sock, _Other} ->
			?LOG_WARNING("tcp error treating headers: ~p, send bad request error back", [_Other]),
			misultin_socket:send(Sock, build_error_message(400, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
			handle_keepalive(Req#req.connection, C, Req);
		{tcp_closed, _Socket} ->
			?LOG_DEBUG("tcp connection was closed, exit", []),
			tcp_closed;
		{ssl_closed, _Socket} ->
			?LOG_DEBUG("ssl tcp connection was closed, exit", []),
			ssl_closed;
		shutdown ->
			?LOG_DEBUG("shutdown message received from server, exit", []),
			misultin_socket:close(Sock, SocketMode);
		_Other ->
			?LOG_DEBUG("received unknown message: ~p, ignoring", [_Other]),
			ignored
	after RecvTimeout ->
		?LOG_DEBUG("headers timeout, sending request timeout error", []),
		misultin_socket:send(Sock, build_error_message(408, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
		handle_keepalive(close, C, Req)
	end.

% default connection
-spec default_connection(http_version()) -> http_connection().
default_connection({1,1}) -> keep_alive;
default_connection(_) -> close.

% Shall we keep the connection alive? Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
-spec keep_alive(http_version(), string()) -> http_connection().
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
keep_alive({0,9}, _)	-> close;
keep_alive(_Vsn, _KA)	-> close.

% Build uri & args in Req
-spec get_uri_and_args(Req::#req{}) -> #req{} | {error, non_neg_integer()}.
get_uri_and_args(Req) ->
	case Req#req.uri of
		{abs_path, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			Req#req{args = Args, uri = {abs_path, F}};
		{absoluteURI, http, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			Req#req{args = Args, uri = {absoluteURI, F}};
		{absoluteURI, https, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			Req#req{args = Args, uri = {absoluteURI, F}};
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			{error, 501};
		{scheme, _Scheme, _RequestString} ->
			{error, 510};
		_ ->
			{error, 403}
	end.

% dispatch operations according to defined method
-spec method_dispatch(C::#c{}, Req::#req{}) -> ok | ignored | ssl_closed | tcp_closed | true.
method_dispatch(C, #req{method = Method} = Req) when Method =:= 'GET'; Method =:= 'POST'; Method =:= 'HEAD'; Method =:= 'PUT'; Method =:= 'DELETE'; Method =:= 'CONNECT'; Method =:= 'OPTIONS' ->
	?LOG_DEBUG("~p request received", [Method]),
	% read body & dispatch
	read_body_dispatch(C, Req);
method_dispatch(C, #req{method = Method} = Req) when Method =:= 'TRACE' ->
	?LOG_DEBUG("~p request received", [Method]),
	% an entity-body is explicitly forbidden in TRACE requests <http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.8>
	main_dispatcher(C, Req),
	handle_keepalive(Req#req.connection, C, Req);
method_dispatch(C, #req{socket = Sock, socket_mode = SocketMode, method = _Method, connection = Connection} = Req) ->
	?LOG_DEBUG("method not implemented: ~p", [_Method]),
	misultin_socket:send(Sock, build_error_message(501, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
	handle_keepalive(Connection, C, Req).

% read body and dispatch to mfa if ok
-spec read_body_dispatch(C::#c{}, Req::#req{}) -> ok | ignored | ssl_closed | tcp_closed | true.	
read_body_dispatch(#c{auto_recv_body = AutoRecvBody} = C, #req{socket = Sock, socket_mode = SocketMode} = Req) ->
	case AutoRecvBody of
		true ->
			?LOG_DEBUG("automatically read body of request",[]),
			% read post body
			case read_body(full, C, Req) of
				{ok, Bin} ->
					?LOG_DEBUG("full body read: ~p", [Bin]),
					Req0 = Req#req{body = Bin},
					main_dispatcher(C, Req0),
					handle_keepalive(Req#req.connection, C, Req0);
				{error, timeout} ->
					?LOG_WARNING("request timeout, sending error", []),
					misultin_socket:send(Sock, build_error_message(408, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
					handle_keepalive(close, C, Req);			
				{error, post_max_size} ->	
					?LOG_WARNING("post request entity too large", []),				
					misultin_socket:send(Sock, build_error_message(413, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
					handle_keepalive(close, C, Req);
				{error, Reason} ->
					?LOG_ERROR("tcp error treating post data: ~p, send bad request error back", [Reason]),
					misultin_socket:send(Sock, build_error_message(400, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
					handle_keepalive(close, C, Req)
			end;
		false ->
			?LOG_DEBUG("auto_recv_body set to false, do not read body of request",[]),
			main_dispatcher(C, Req),
			handle_keepalive(Req#req.connection, C, Req)
	end.			

% read the body
-spec read_body(Mode::full | chunk, C::#c{}, Req::#req{}) -> {ok | chunk, Body::binary()} | end_of_chunks | {error, Reason::term()}.
read_body(Mode, C, Req) ->
	case read_post_body(C, Req) of
		{ok, Bin} ->
			?LOG_DEBUG("full body read: ~p", [Bin]),
			{ok, Bin};
		chunked_body ->
			?LOG_DEBUG("body is chunked, proceed to reading it",[]),
			read_post_body_chunk(Mode, C, Req, <<>>);
		{error, Reason} ->
			?LOG_ERROR("error reading body: ~p", [Reason]),
			{error, Reason}
	end.

% read post body chunk
-spec read_post_body_chunk(Mode::full | chunk, C::#c{}, Req::#req{}, Acc::binary()) -> {ok | chunk, binary()} | end_of_chunks | {error, Reason::term()}.
read_post_body_chunk(Mode, #c{post_max_size = PostMaxSize} = C, Req, Acc) ->
	case read_post_body_chunk_headline(C, Req) of
		{chunk, _Chunk} when Mode =:= full, size(Acc) > PostMaxSize ->
			?LOG_DEBUG("total size of chunked parts of ~p bytes exceed limit of ~p bytes", [size(Acc), PostMaxSize]),
			{error, post_max_size};
		{chunk, Chunk} when Mode =:= full ->
			?LOG_DEBUG("received a chunk: ~p, proceed to automatic reading of next chunk", [Chunk]),
			read_post_body_chunk(Mode, C, Req, <<Acc/binary, Chunk/binary>>);
		{chunk, Chunk} ->
			?LOG_DEBUG("received a chunk: ~p, return it", [Chunk]),
			{chunk, Chunk};
		end_of_chunks when Mode =:= full ->
			?LOG_DEBUG("finished reading chunks",[]),
			{ok, Acc};
		end_of_chunks ->
			end_of_chunks;
		{error, Reason} ->
			{error, Reason}
	end.

% Read the post body according to headers
-spec read_post_body(C::#c{}, Req::#req{}) -> {ok, Body::binary()} | chunked_body | {error, Reason::term()}.
-spec read_post_body(C::#c{}, Req::#req{}, Out::{error, no_valid_content_specs} | {ok, <<>>}) -> {ok, Body::binary()} | chunked_body | {error, Reason::term()}.
read_post_body(C, #req{method = Method} = Req) when Method =:= 'POST'; Method =:= 'PUT' ->
	% on PUT and POST require content length or transfer encoding headers
	read_post_body(C, Req, {error, no_valid_content_specs});
read_post_body(C, Req) ->
	% on other requests just set body to <<>> in case of error
	read_post_body(C, Req, {ok, <<>>}).
read_post_body(C, #req{content_length = ContentLength} = Req, NoContentNoChunkOutput) ->
	case ContentLength of
		undefined ->
			% no specified content length, check transfer encoding header
			case misultin_utility:get_key_value('Transfer-Encoding', Req#req.headers) of
				undefined ->
					NoContentNoChunkOutput;
				TE ->
					case string:to_lower(TE) of
						"chunked" ->
							?LOG_DEBUG("chunked content being sent by the client",[]),
							chunked_body;
					_ ->
						NoContentNoChunkOutput
				end
			end;
		Len ->
			i_read_post_body(C, Req, Len)
	end.

% Read body
-spec i_read_post_body(C::#c{}, Req::#req{}, Len::non_neg_integer()) -> {ok, Body::binary()} | {error, Reason::term()}.
i_read_post_body(#c{recv_timeout = RecvTimeout, post_max_size = PostMaxSize}, #req{socket = Sock, socket_mode = SocketMode} = _Req, Len) ->
	% check if content length has been provided
	case Len of
		0 ->
			?LOG_DEBUG("zero content-lenght specified, skipping parsing body of request", []),
			{ok, <<>>};
		Len when Len =< PostMaxSize ->
			?LOG_DEBUG("content length has been specified, parsing content in body of request", []),
			misultin_socket:setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
			case misultin_socket:recv(Sock, Len, RecvTimeout, SocketMode) of
				{ok, Bin} ->
					{ok, Bin};
				{error, timeout} ->
					{error, timeout};
				Other ->
					{error, Other}
			end;
		_Len ->
			?LOG_DEBUG("content length specified of ~p bytes exceed limit of ~p bytes", [_Len, PostMaxSize]),
			{error, post_max_size}
	end.

% Read body chunks
-spec read_post_body_chunk_headline(C::#c{}, Req::#req{}) -> {chunk, Body::binary()} | end_of_chunks | {error, Reason::term()}.
-spec read_post_body_chunk_content(C::#c{}, Req::#req{}, Len::non_neg_integer()) -> {chunk, Body::binary()} | end_of_chunks | {error, Reason::term()}.
read_post_body_chunk_headline(#c{recv_timeout = RecvTimeout} = C, #req{socket = Sock, socket_mode = SocketMode} = Req) ->
	misultin_socket:setopts(Sock, [{packet, line}, {active, false}], SocketMode),
	case misultin_socket:recv(Sock, 0, RecvTimeout, SocketMode) of
		{ok, HeadLineBin} ->
			?LOG_DEBUG("received a chunked headline: ~p", [HeadLineBin]),
			case get_chunk_length(binary_to_list(HeadLineBin)) of			
				{ok, Len} ->
					read_post_body_chunk_content(C, Req, Len);
				{error, Reason} ->
					{error, Reason}
			end;
		{error, timeout} ->
			{error, timeout};
		Other ->
			{error, Other}
	end.
read_post_body_chunk_content(#c{recv_timeout = RecvTimeout}, #req{socket = Sock, socket_mode = SocketMode}, Len) ->
	?LOG_DEBUG("receiving a chunk of ~p bytes", [Len]),
	misultin_socket:setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
	case misultin_socket:recv(Sock, Len + 2, RecvTimeout, SocketMode) of	% we need 2 more bytes for the chunked CRLF closing
		{ok, Bin} ->
			?LOG_DEBUG("received chunk content, with 2 additional chunked closing bytes CRLF: ~p", [Bin]),
			case Len of
				0 -> 
					?LOG_DEBUG("client has finished sending chunks",[]),
					end_of_chunks;
				_ ->
					% continue with next chunk
					?LOG_DEBUG("waiting for next chunk", []),
					{chunk, <<Bin:Len/binary>>}
			end;
		{error, timeout} ->
			{error, timeout};
		Other ->
			{error, Other}
	end.

% Get length of the next chunk.
-spec get_chunk_length(HeadLine::string()) -> {ok, Len::non_neg_integer()} | {error, Reason::term()}.
get_chunk_length(HeadLine) ->
	% take away CRLF
	HeadLine0 = case string:rchr(HeadLine, $\r) of
		0 -> HeadLine;
		Pos -> string:substr(HeadLine, 1, Pos - 1)
	end,
	% take away ;
	Tokens = string:tokens(HeadLine0, ";"),
	case catch lists:nth(1, Tokens) of
		{'EXIT', _} ->
			{error, chunked_headline_empty};
		HeadLenStr ->
			case catch erlang:list_to_integer(HeadLenStr, 16) of
				{'EXIT', _} -> {error, invalid_chunked_headline};
				Len -> {ok, Len}
			end
	end.

% handle the request and get back to the request loop
-spec handle_keepalive(http_connection(), C::#c{}, Req::#req{}) -> ok | ignored | ssl_closed | tcp_closed | true.
handle_keepalive(close, _C, #req{socket = Sock, socket_mode = SocketMode} = _Req) ->
	misultin_socket:close(Sock, SocketMode);
handle_keepalive(keep_alive, C, #req{socket = Sock, socket_mode = SocketMode} = Req) ->
	?MODULE:request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert}).

% File dispatcher
main_dispatcher(#c{static = false} = C, Req) ->
	% no static option specified
	process_dispatcher(C, Req);
main_dispatcher(#c{static = StaticDir, loop = Loop} = C, Req) ->
	% static directory specified
	StaticLoop = fun(ReqT) -> handle_static(ReqT, StaticDir, Loop) end,
	% replace original loop with static loop
	process_dispatcher(C#c{loop = StaticLoop}, Req).

% Process dispatcher
-spec process_dispatcher(C::#c{}, Req::#req{}) -> closed | true.
process_dispatcher(#c{table_date_ref = TableDateRef, loop = Loop, autoexit = AutoExit} = C, Req) ->
	% spawn_link custom loop
	Self = self(),
	% trap exit
	process_flag(trap_exit, true),
	% spawn
	LoopPid = spawn_link(fun() ->
		% create request
		ReqT = {misultin_req, Self, TableDateRef},
		% start custom loop
		Loop(ReqT)
	end),
	% enter loop
	{HttpCode, SizeSent} = socket_loop(C, Req, LoopPid, #req_options{}, [], none, 0),
	% unlink
	process_flag(trap_exit, false),
	erlang:unlink(LoopPid),
	% build access log
	case HttpCode of
		none ->
			% do nothing
			ok;
		_ ->
			% build access log
			build_access_log(Req, HttpCode, SizeSent, C#c.table_date_ref, C#c.access_log)
	end,
	% close http loop or send message to LoopPid
	loop_close(LoopPid, AutoExit).

% socket loop
-spec socket_loop(C::#c{}, Req::#req{}, LoopPid::pid(), #req_options{}, AppHeaders::http_headers(), HttpCodeSent::non_neg_integer() | none, SizeSent::non_neg_integer()) -> {HttpCode::non_neg_integer() | none, SizeSent::non_neg_integer()}.
socket_loop(#c{compress = Compress} = C, #req{socket = Sock, socket_mode = SocketMode, headers = RequestHeaders} = Req, LoopPid, #req_options{comet = Comet} = ReqOptions, AppHeaders, HttpCodeSent, SizeSent) ->
	% are we trapping client tcp close events?
	case Comet of
		true ->
			% set to active in order to receive closed events
			misultin_socket:setopts(Sock, [{active, once}], SocketMode);
		_ ->
			ok
	end,
	% receive
	receive
		{response, HttpCode, Headers0, Body} ->
			% received normal response
			?LOG_DEBUG("sending normal response", []),
			% build binary body & compress if necessary
			{CompressHeaders, BodyBinary} = compress_body(RequestHeaders, convert_to_binary(Body), Compress),
			% encode headers & extra headers
			Headers = add_headers(Headers0, BodyBinary, C#c.table_date_ref, Req),			
			Enc_headers = enc_headers(lists:append([CompressHeaders, AppHeaders, Headers])),
			% build and send response
			Resp = [misultin_utility:get_http_status_code(HttpCode, Req#req.vsn), Enc_headers, <<"\r\n">>, BodyBinary],
			% send
			misultin_socket:send(Sock, Resp, SocketMode),
			% loop and save sent status
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCode, size(BodyBinary));
		{CallerPid, {reqinfo, ReqInfo}} ->
			ReqResponse = case ReqInfo of
				raw				-> Req;
				socket			-> Req#req.socket;
				socket_mode		-> Req#req.socket_mode;
				peer_addr		-> Req#req.peer_addr;
				peer_port		-> Req#req.peer_port;
				peer_cert		-> Req#req.peer_cert;
				connection		-> Req#req.connection;
				content_length	-> Req#req.content_length;
				vsn				-> Req#req.vsn;
				method			-> Req#req.method;
				uri				-> Req#req.uri;
				args			-> Req#req.args;
				headers			-> Req#req.headers;
				body			-> Req#req.body
			end,
			?LOG_DEBUG("received request info for: ~p, responding with ~p", [ReqInfo, ReqResponse]),
			misultin_utility:respond(CallerPid, ReqResponse),
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent);
		{CallerPid, {session_cmd, SessionCmd}} ->
			?LOG_DEBUG("received a session command: ~p", [SessionCmd]),
			case misultin_utility:get_peer(Req#req.headers, Req#req.peer_addr) of
				{error, Reason} ->
					?LOG_DEBUG("error getting remote peer_addr: ~p, cannot get/create session", [Reason]),
					misultin_utility:respond(CallerPid, {error, {peer_addr, Reason}}),
					socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent);
				{ok, PeerAddr} ->
					?LOG_DEBUG("got remote peer_addr: ~p", [PeerAddr]),
					case SessionCmd of
						{session, Cookies} ->
							% start new session process or retrieve exiting one's id
							{SessionId, _SessionVars} = SessionInfo = misultin_sessions:session(C#c.sessions_ref, Cookies, PeerAddr),                                                                                                                                                           
							?LOG_DEBUG("got session info: ~p", [SessionInfo]),                                                                             
							% respond with session id                                                                                                      
							misultin_utility:respond(CallerPid, SessionInfo),                                                                              
							% add session id cookie header                                                                                                 
							socket_loop(C, Req, LoopPid, ReqOptions, [misultin_sessions:set_session_cookie(SessionId)|AppHeaders], HttpCodeSent, SizeSent);
						{save_session_state, SessionId, SessionState} ->
							% save session state
							?LOG_DEBUG("trying to save new session state: ~p", [SessionState]),
							Response = misultin_sessions:save_session_state(C#c.sessions_ref, SessionId, SessionState, PeerAddr),
							misultin_utility:respond(CallerPid, Response),
							% loop
							socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent)
					end
			end;
		{CallerPid, body_recv} ->
			?LOG_DEBUG("received a request to manually read the body",[]),			
			misultin_utility:respond(CallerPid, read_body(chunk, C, Req)),
			% loop
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent);
		{set_cookie, CookieHeader} ->
			?LOG_DEBUG("received a cookie: ~p", [CookieHeader]),
			% add cookie header
			socket_loop(C, Req, LoopPid, ReqOptions, [CookieHeader|AppHeaders], HttpCodeSent, SizeSent);
		{set_option, {comet, OptionVal}} ->
			?LOG_DEBUG("setting request option comet to ~p", [OptionVal]),
			socket_loop(C, Req, LoopPid, ReqOptions#req_options{comet = OptionVal}, AppHeaders, HttpCodeSent, SizeSent);
		{stream_head, HttpCode, Headers0} ->
			?LOG_DEBUG("sending stream head", []),
			Headers = add_headers_no_content_length(Headers0, C#c.table_date_ref, Req),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode, Req#req.vsn), Enc_headers, <<"\r\n">>],
			% respond
			misultin_socket:send(Sock, Resp, SocketMode),
			% loop and set HttpCode
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCode, SizeSent);
		{stream_data, Data} ->
			?LOG_DEBUG("sending stream data", []),
			% respond
			misultin_socket:send(Sock, Data, SocketMode),
			% loop and add to size sent
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent + erlang:iolist_size(Data));
		stream_close ->
			?LOG_DEBUG("closing stream", []),
			misultin_socket:close(Sock, SocketMode),
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent);
		{stream_error, 404} ->
			?LOG_ERROR("file not found", []),
			misultin_socket:send(Sock, build_error_message(404, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
			% we already build an access log so reset HttpCodeSent & SizeSent
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, none, 0);
		{stream_error, _Reason} ->
			?LOG_ERROR("error sending stream: ~p", [_Reason]),
			misultin_socket:send(Sock, build_error_message(500, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
			misultin_socket:close(Sock, SocketMode),
			% we already build an access log so reset HttpCodeSent & SizeSent
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, none, 0);
		{tcp_closed, Sock} ->
			?LOG_DEBUG("client closed socket",[]),
			{HttpCodeSent, SizeSent};
		{ssl_closed, Sock} ->
			?LOG_DEBUG("client closed ssl socket",[]),
			{HttpCodeSent, SizeSent};
		shutdown ->
			?LOG_DEBUG("shutdown message received from server",[]),
			{HttpCodeSent, SizeSent};			
		{'EXIT', LoopPid, normal} ->
			?LOG_DEBUG("normal finishing of custom loop",[]),
			{HttpCodeSent, SizeSent};
		{'EXIT', LoopPid, _Reason} ->
			?LOG_ERROR("error in custom loop: ~p serving request: ~p", [_Reason, Req]),
			misultin_socket:send(Sock, build_error_message(500, Req, C#c.table_date_ref, C#c.access_log), SocketMode),
			{none, 0};
		{SocketMode, Sock, _HttpData} ->
			?LOG_WARNING("received http data from client when running a comet application [client should normally not send messages]: ~p, sending error and closing socket", [_HttpData]),
			misultin_socket:send(Sock, build_error_message(400, Req#req{connection = close}, C#c.table_date_ref, C#c.access_log), SocketMode),
			misultin_socket:close(Sock, SocketMode),
			{none, 0};
		_Else ->
			?LOG_WARNING("received unexpected message in socket_loop: ~p, ignoring", [_Else]),
			socket_loop(C, Req, LoopPid, ReqOptions, AppHeaders, HttpCodeSent, SizeSent)
	end.

% Close socket and custom handling loop dependency
-spec loop_close(LoopPid::pid(), AutoExit::boolean()) -> closed | true.
loop_close(LoopPid, AutoExit) ->
	case AutoExit of
		true ->
			% kill handling loop process
			?LOG_DEBUG("ensure the exiting of the http handling loop",[]),
			exit(LoopPid, kill);
		false ->
			% the killing of the http handling loop process is handled in the loop itself -> send event
			?LOG_DEBUG("send client closed event the http handling loop",[]),
			LoopPid ! closed
	end.

% Ensure Body is binary.
-spec convert_to_binary(list() | binary() | atom()) -> binary().
convert_to_binary(Body) when is_binary(Body) ->
	Body;
convert_to_binary(Body) when is_list(Body) ->
	list_to_binary(lists:flatten(Body));
convert_to_binary(Body) when is_atom(Body) ->
	list_to_binary(atom_to_list(Body)).

% add necessary headers
-spec add_headers(http_headers(), BodyBinary::binary(), TableDateRef::ets:tid(), Req::#req{}) -> http_headers().
add_headers(OriginalHeaders, BodyBinary, TableDateRef, Req) ->
	Headers = add_output_header('Content-Length', {OriginalHeaders, BodyBinary}),
	add_headers_no_content_length(Headers, TableDateRef, Req).

% add necessary headers without content length one
-spec add_headers_no_content_length(Headers::http_headers(), TableDateRef::ets:tid(), Req::#req{}) -> http_headers().
add_headers_no_content_length(Headers, TableDateRef, Req) ->
	Headers1 = add_output_header('Connection', {Headers, Req}),
	Headers2 = add_output_header('Server', Headers1),
	add_output_header('Date', {Headers2, TableDateRef}).

% Add necessary Content-Length Header
-spec add_output_header(http_header(), term()) -> http_headers().
add_output_header('Content-Length', {Headers, Body}) ->
	case misultin_utility:get_key_value('Content-Length', Headers) of
		undefined ->
			[{'Content-Length', size(Body)}|Headers];
		_ ->
			Headers
	end;
% Add necessary Connection Header
add_output_header('Connection', {Headers, Req}) ->
	% echo
	case misultin_utility:get_key_value('Connection', Headers) of
		undefined ->
			[{'Connection', connection_str(Req#req.connection)}|Headers];
		_ ->
			Headers
	end;
% Add necessary Server header
add_output_header('Server', Headers) ->
	case misultin_utility:get_key_value('Server', Headers) of
		undefined ->
			[{'Server', ?SERVER_VERSION_TAG}|Headers];
		_ ->
			Headers
	end;
% Add necessary Date header
add_output_header('Date', {Headers, TableDateRef}) ->
	case misultin_utility:get_key_value('Date', Headers) of
		undefined ->
			% get header from gen_server
			RfcDate = misultin_server:get_rfc_date(TableDateRef),
			[{'Date', RfcDate}|Headers];
		_ ->
			Headers
	end.

% Helper to Connection string
-spec connection_str(http_connection()) -> string().
connection_str(keep_alive) -> "Keep-Alive";
connection_str(close) -> "Close".

% Encode headers
-spec enc_headers(http_headers() | term()) -> string().
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
	[atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
	[Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_binary(Tag) ->
	[binary_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
	[];
enc_headers([_Other|T]) ->
	% not a tuple format, ignore
	enc_headers(T).
-spec enc_header_val(Val::atom() | integer() | string()) -> string().
enc_header_val(Val) when is_atom(Val) ->
	atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
	erlang:integer_to_list(Val);
enc_header_val(Val) ->
	Val.

% Split the path at the ?
-spec split_at_q_mark(string(), Acc::string()) -> {H::string(), T::string()}.
split_at_q_mark([$?|T], Acc) ->
	{lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
	split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
	{lists:reverse(Acc), []}.

% Compress body depending on Request Headers and misultin supported encodings.
-spec compress_body(RequestHeaders::http_headers(), BodyBinary::binary(), boolean()) -> {EncodingHeaders::http_headers(), CompressedBody::binary()}.
compress_body(RequestHeaders, BodyBinary, true) ->
	case misultin_utility:get_key_value('Accept-Encoding', RequestHeaders) of
		undefined ->
			% unkown encoding accepted, return body as is
			?LOG_DEBUG("no accepted encoding specified by request: building binary body without compression", []),
			{[], BodyBinary};
		AcceptEncoding ->
			case set_encoding(AcceptEncoding) of
				none ->
					% no common encoding accepted
					?LOG_DEBUG("no supported compression: building binary body without compression", []),
					{[], BodyBinary};
				Encoding ->
					?LOG_DEBUG("building binary body with ~p compression", [Encoding]),
					{[{'Content-Encoding', Encoding}],
					encode(Encoding, BodyBinary)}
			end
	end;
compress_body(_RequestHeaders, BodyBinary, false) ->
	?LOG_DEBUG("building binary body without compression", []),
	{[], BodyBinary}.
			
% Compress body.
-spec encode(EncodeType::http_supported_encoding(), Body::binary()) -> CompressedBody::binary().
encode(deflate, BodyBinary) ->
	zlib:compress(BodyBinary);
encode(gzip, BodyBinary) ->
	zlib:gzip(BodyBinary).

% Set encoding name depending on Request Headers and supported misultin encodings.
-spec set_encoding(AcceptEncodingHeader::string()) -> http_supported_encoding() | none.
set_encoding(AcceptEncodingHeader) ->
	% get request accepted encodings
	RequestEncodings = get_accepted_encodings(AcceptEncodingHeader),
	% get a request accepted encoding which is supported by misultin
	F = fun({E, _Q}) ->
		lists:member(E, ?SUPPORTED_ENCODINGS)
	end,
	case lists:filter(F, RequestEncodings) of
		[] -> none;
		[{Enc, _Q}|_T] -> list_to_atom(Enc)
	end.

% Get accepted encodings and quality, sorted by quality.
-spec get_accepted_encodings(AcceptEncodingHeader::string()) -> gen_proplist().
get_accepted_encodings(AcceptEncodingHeader) ->
	% take away empty spaces
	Header = lists:filter(fun(E) -> case E of $\s -> false; _ -> true end end, AcceptEncodingHeader),
	% get values
	F = fun(E, AccIn) ->
		case string:tokens(E, ";") of
			[Enc] -> [{Enc, 1.0}|AccIn];
			[Enc, QStr] ->
				[_, Val] = string:tokens(QStr, "="),
				case list_to_number(Val) of
					not_a_number -> AccIn;
					V -> [{Enc, V}|AccIn]
				end;
			_ -> AccIn
		end
	end,
	Encodings0 = lists:foldl(F, [], string:tokens(Header, ",")),
	% sort
	lists:sort(fun({_E1, Q1}, {_E2, Q2}) -> Q1 > Q2 end, Encodings0).
			
% Converts a list to a number.
-spec list_to_number(list()) -> integer() | not_a_number.
list_to_number(L) ->
	case catch list_to_float(L) of
		{'EXIT', _} ->
			case catch erlang:list_to_integer(L) of
				{'EXIT', _} -> not_a_number;
				Value -> Value
			end;
		Value -> Value
	end.

% build access log
-spec build_access_log(Req::#req{}, HttpCode::non_neg_integer() | atom(), ContentLength::non_neg_integer() | atom(), TableDateRef::ets:tid(), AccessLogFun::function()) -> ok.
build_access_log(Req, HttpCode, ContentLength, TableDateRef, AccessLogFun) ->
	{PeerAddr, DateTime, RequestLine} = build_access_data(Req, TableDateRef),
	case AccessLogFun of
		undefined -> ok;
		_ ->
			AccessInfo = {PeerAddr, DateTime, RequestLine, HttpCode, ContentLength},
			?LOG_DEBUG("sending access log information to access log fun: ~p", [AccessInfo]),
			AccessLogFun(AccessInfo)
	end,
	?LOG_INFO("~s - - [~s] \"~s\" ~p ~p", [PeerAddr, DateTime, RequestLine, HttpCode, ContentLength]),
	ok.

-spec build_access_data(Req::#req{}, TableDateRef::ets:tid()) -> {PeerAddr::string(), DateTime::string(), RequestLine::string()}.
build_access_data(#req{uri = undefined} = Req, TableDateRef) ->
	build_access_data_do(Req#req.peer_addr, "no-parsing-done", TableDateRef);
build_access_data(Req, TableDateRef) ->
	{Maj, Min} = Req#req.vsn,
	FullUri = lists:flatten(io_lib:format("~s ~s HTTP/~p.~p", [Req#req.method, build_full_uri(misultin_req:uri_unquote(Req#req.uri), Req#req.args), Maj, Min])),
	build_access_data_do(Req#req.peer_addr, FullUri, TableDateRef).

-spec build_access_data_do(PeerAddr::inet:ip_address(), RequestLine::string(), TableDateRef::ets:tid()) -> {PeerAddr::string(), DateTime::string(), RequestLine::string()}.
build_access_data_do(PeerAddr, RequestLine, TableDateRef) ->
	{
		misultin_utility:convert_ip_to_list(PeerAddr),		% ip peer address
		misultin_server:get_iso8601_date(TableDateRef),		% datetime
		RequestLine											% request line
	}.
	
-spec build_full_uri(Uri::string(), Args::string()) -> string().
build_full_uri(Uri, []) -> Uri;
build_full_uri(Uri, Args) -> lists:concat([Uri, "?", Args]).

% ---------------------------- \/ Static File Handler ------------------------------------------------------

% test if this is a static file request, otherwise fallback to original loop
handle_static(Req, StaticDir, Loop) ->
	?LOG_DEBUG("checking if this is a request to a static file",[]),
	handle_static(Req:get(method), Req:resource([urldecode]), Req, StaticDir, Loop).
handle_static('GET', ["static"], Req, _StaticDir, _Loop) ->
	Req:respond(403, build_error_message_body(403));
handle_static('GET', ["static" | FilePath], Req, StaticDir, _Loop) ->
	?LOG_DEBUG("static request found, sanitizing path",[]),
	case misultin_utility:sanitize_path_tokens(FilePath) of
		invalid ->
			?LOG_DEBUG("invalid static request :~p", [FilePath]),
			Req:respond(403);
		SanitizedPath ->
			FullFilePath = filename:join([StaticDir, filename:join(SanitizedPath)]),
			?LOG_DEBUG("sending file in path: ~p", [FullFilePath]),	
			Req:file(FullFilePath)
	end;
handle_static(_, _, Req, _, Loop) ->
	Loop(Req).

% ---------------------------- /\ Static File Handler ------------------------------------------------------

% ============================ /\ INTERNAL FUNCTIONS =======================================================
