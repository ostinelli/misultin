% ==========================================================================================================
% MISULTIN - Include file
% 
% Copyright (C) 2009, Sean Hinde, Roberto Ostinelli <roberto@ostinelli.net>
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
% macros
-define(INTERNAL_SERVER_ERROR_500, "HTTP/1.1 500 Internal Server Error\r\n\r\n").
-define(NOT_IMPLEMENTED_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(FORBIDDEN_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(NOT_FOUND_404, "HTTP/1.1 404 Not Found\r\n\r\n").
-define(CONTENT_LENGTH_REQUIRED_411, "HTTP/1.1 411 Length Required\r\n\r\n").
-define(REQUEST_TIMEOUT_408, "HTTP/1.1 408 Request Timeout\r\n\r\n").

% define debug
-ifdef(debug).
-define(DEBUG(Level, Str, Args),
	% Level = error | warning | info | debug
	case Level of
		error ->
			erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
		warning ->
			case ?debug of
				debug ->
					erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				info ->
					erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				warning	->
					erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				_ ->
					ok
			end;
		info ->
			case ?debug of
				debug ->
					erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				info ->
					erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				_ ->
					ok
			end;
		debug ->
			case ?debug of
				debug ->
					erlang:apply(error_logger, info_msg, [lists:concat(["[DEBUG]	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]);
				_ ->
					ok
			end;			
		_ ->
			ok
	end
).
-else.
-define(DEBUG(Level, Str, Args), true).
-endif.


% Request
-record(req, {
	peer_addr,					% peer IP | undefined
	peer_port,					% peer port | undefined
	connection = keep_alive,	% keep_alive | close
	content_length,				% Integer
	vsn,						% {Maj,Min}
	method,						% 'GET'|'POST'
	uri,						% Truncated URI /index.html
	args = "",					% Part of URI after ?
	headers,					% [{Tag, Val}]
	body = <<>>					% Content Body
}).
