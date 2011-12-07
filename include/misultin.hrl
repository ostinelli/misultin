% ==========================================================================================================
% MISULTIN - Include file
% 
% Copyright (C) 2011, Sean Hinde, Roberto Ostinelli <roberto@ostinelli.net>
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%    following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%    products derived from this software without specific prior written permission.
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


% ============================ \/ LOG ======================================================================
-ifdef(log_debug).
-define(LOG_DEBUG(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["[DEBUG]	pid: ", pid_to_list(self()), "~n	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_info).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_error).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), ok).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
% default to warning level
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-endif.
-endif.
-endif.
% ============================ /\ LOG ======================================================================


% ============================ \/ TYPES ====================================================================

% ---------------------------- \/ MISULTIN -----------------------------------------------------------------
-type misultin_option_tcp() ::
	{ip, string() | tuple()} |
	{port, non_neg_integer()} |
	{backlog, non_neg_integer()} |
	{acceptors_poolsize, non_neg_integer()} |
	{recv_timeout, non_neg_integer()} |
	{max_connections, non_neg_integer()} |
	{ssl, gen_proplist()} |
	{recbuf, non_neg_integer()}.
-type misultin_option_server() :: 
	{name, atom()} |
	{post_max_size, non_neg_integer()} |
	{get_url_max_size, non_neg_integer()} |
	{compress, boolean()} |
	{loop, function()} |
	{autoexit, boolean()} |
	{ws_loop, undefined | function()} |
	{ws_autoexit, boolean()} |
	{ws_versions, [websocket_version()]} |
	{sessions_expire, non_neg_integer()} |
	{access_log, undefined | function()} |
	{auto_recv_body, boolean()}.
-type misultin_option() :: misultin_option_tcp() |  misultin_option_server().
% ---------------------------- /\ MISULTIN -----------------------------------------------------------------

% ---------------------------- \/ HTTP ---------------------------------------------------------------------
-type http_version() :: {Maj::non_neg_integer(), Min::non_neg_integer()}.

-type http_header() :: 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' |
	'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 'From' |
	'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' |
	'Max-Forwards' | 'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' |
	'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' | 'Www-Authenticate' |
	'Allow' | 'Content-Base' | 'Content-Encoding' | 'Content-Language' | 'Content-Length' | 'Content-Location' |
	'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 'Last-Modified' | 'Accept-Ranges' |
	'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 'Keep-Alive' | 'Proxy-Connection' |
	list() | binary().
-type http_headers() :: list({http_header(), list() | binary() | integer() | atom()}).

-type http_method() :: 'GET' | 'POST' | 'HEAD' | 'PUT' | 'DELETE' | 'TRACE' | 'CONNECT'.

-type http_connection() :: close | keep_alive.

-type http_uri() ::
	{abs_path, Path::list()} |
	{absoluteURI, Path::list()} |
	{absoluteURI, http | https | atom(), Host::binary(), Port::non_neg_integer(), Path::list()} |
	{scheme, Scheme::list(), RequestString::list()}.
	
-type http_supported_encoding() :: deflate | gzip.
% ---------------------------- /\ HTTP ---------------------------------------------------------------------

% ---------------------------- \/ OTHER --------------------------------------------------------------------
-type websocket_version() ::
	'draft-hybi-10' |
	'draft-hybi-17' |
	'draft-hixie-68' |
	'draft-hixie-76'.

-opaque websocket_state() :: term().

-type socketmode() :: http | ssl.
-type socket() :: inet:socket() | term().	% unfortunately ssl does not export the socket equivalent, we could use {sslsocket, term(), term()} but this is relying on internals.

-type cookies_options() :: [
	{max_age, undefined | integer()} |
	{local_time, undefined | date_tuple()} |
	{secure, true | term()} |
	{domain, undefined | string()} |
	{path, undefined | string()} |
	{http_only, true | term()}
].

-type gen_proplist() :: [{Tag::atom()|list()|binary(), Value::term()}].
-type gen_proplist_options() :: [{Tag::atom()|list()|binary(), Value::term()} | atom()].

-type date_tuple() :: {{non_neg_integer(), 1..12, 1..31}, {0..24, 0..60, 0..60}}.
% ---------------------------- /\ OTHER --------------------------------------------------------------------

% ============================ /\ TYPES ====================================================================


% ============================ \/ RECORDS ==================================================================

% misultin server Options
-record(custom_opts, {
	post_max_size		= undefined :: undefined | non_neg_integer(),	% maximum post size in bytes, defaults to 4 MB
	get_url_max_size	= undefined :: undefined | non_neg_integer(),	% maximum GET url size in bytes, defaults to 2000
	compress			= false :: boolean(),							% send compressed output if supported by browser
	loop				= undefined :: undefined | function(),			% the fun handling requests
	autoexit			= true :: boolean(),							% shoud the http process be automatically killed?
	ws_loop				= undefined :: undefined | function(),			% the loop handling websockets
	ws_autoexit			= true :: boolean(),							% shoud the ws process be automatically killed?
	ws_versions			= undefined :: [websocket_version()],			% list of supported ws versions
	access_log			= undefined :: undefined | function(),			% access log function
	ws_force_ssl		= false :: boolean(),							% if we are deployed behind stunnel, or other ssl proxy
	proxy_protocol		= false :: boolean(),							% upstream proxy is sending us http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt
	auto_recv_body		= true :: boolean(),							% if set to false, body has to be manually read in loop
	static				= false :: boolean()							% if set to a directory, then all files in it will be automatically sent to the browser.
}).

% Request
-record(req, {
	socket			= undefined :: undefined | socket(),			% the socket handling the request
	socket_mode		= http :: socketmode(),
	peer_addr		= undefined :: undefined | inet:ip_address(),	% peer IP | undefined: this is the peer address of the connection, may differ from the client
	peer_port		= undefined :: undefined | non_neg_integer(),	% peer port | undefined: this is the peer port of the connection, may differ from the client
	peer_cert		= undefined :: undefined | term(),				% the DER encoded peer certificate that can be decoded with public_key:pkix_decode_cert/2
	connection		= close :: keep_alive | close,
	content_length	= undefined :: undefined | non_neg_integer(),
	vsn				= {1, 1} :: http_version(),						% defaults to HTTP/1.1
	method			= undefined :: undefined | http_method(),
	uri				= undefined :: undefined | http_uri(),
	args			= "" :: list(),									% Part of URI after ?
	headers			= [] :: http_headers(),
	body			= <<>> :: binary(),
	ws_force_ssl	= false :: boolean()							% if we are deployed behind stunnel, or other ssl proxy
}).

% Websocket Request
-record(ws, {
	socket			= undefined :: undefined | socket(),			% the socket handling the request
	socket_mode		= http :: socketmode(),
	ws_autoexit		= true :: boolean(),							% shoud the ws process be automatically killed?
	peer_addr		= undefined :: undefined | inet:ip_address(),	% peer IP | undefined: this is the peer address of the connection, may differ from the client
	peer_port		= undefined :: undefined | non_neg_integer(),	% peer port | undefined: this is the peer port of the connection, may differ from the client
	peer_cert		= undefined :: undefined | term(),				% the DER encoded peer certificate that can be decoded with public_key:pkix_decode_cert/2
	vsn				= undefined :: undefined | websocket_version(),
	origin			= undefined :: undefined | list(),				% the originator
	host			= undefined :: undefined | list(),				% the host
	path			= undefined :: undefined | list(),				% the websocket GET request path
	headers			= [] :: http_headers()
}).

% ============================ /\ RECORDS ==================================================================
