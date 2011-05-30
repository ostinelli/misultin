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

% define debug
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
-ifdef(log_warning).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), ok).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-endif.
-endif.
-endif.

% misultin server Options
-record(custom_opts, {
	binary_mode,				% binary mode: true | false
	post_max_size,				% maximum post size in bytes, defaults to 4 MB
	get_url_max_size,			% maximum GET url size in bytes, defaults to 2000
	compress,					% send compressed output if supported by browser
	stream_support,				% stream support option
	loop,						% the fun handling requests
	autoexit,					% true | false
	ws_loop,					% the loop handling websockets
	ws_autoexit					% true | false
}).

% Request
-record(req, {
	socket,						% the socket handling the request
	socket_mode,				% http | ssl
	binary_mode,				% binary mode: true | false
	peer_addr,					% peer IP | undefined
	peer_port,					% peer port | undefined
	peer_cert,					% undefined | the DER encoded peer certificate that can be decoded with public_key:pkix_decode_cert/2
	connection = close,			% keep_alive | close
	content_length,				% Integer
	vsn,						% {Maj,Min}
	method,						% 'GET'|'POST'
	uri,						% Truncated URI /index.html
	args = "",					% Part of URI after ?
	headers,					% [{Tag, Val}]
	body = <<>>					% Content Body
}).

% Websocket Request
-record(ws, {
	socket,						% the socket handling the request
	socket_mode,				% http | ssl
	binary_mode,				% binary mode: true | false
	ws_autoexit,				% websocket process is automatically killed: true | false
	peer_addr,					% peer IP | undefined
	peer_port,					% peer port | undefined
	peer_cert,					% undefined | the DER encoded peer certificate that can be decoded with public_key:pkix_decode_cert/2
	vsn,						% {Maj,Min} | {'draft-hixie', Ver}
	origin,						% the originator
	host,						% the host
	path,						% the websocket GET request path
	headers						% [{Tag, Val}]
}).
