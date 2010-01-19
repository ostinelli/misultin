% ==========================================================================================================
% MISULTIN - Main
%
% >-|-|-(°>
% 
% Copyright (C) 2009, Roberto Ostinelli <roberto@ostinelli.net>.
% All rights reserved.
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
-module(misultin_utility).
-vsn('0.3.4').

% API
-export([get_http_status_code/1]).


% ============================ \/ API ======================================================================

% Function: HttpStatus
% Description: Returns a complete HTTP header
% most common first
get_http_status_code(200) ->
	"HTTP/1.1 200 OK\r\n";
get_http_status_code(100) ->
	"HTTP/1.1 100 Continue\r\n";
get_http_status_code(101) ->
	"HTTP/1.1 101 Switching Protocols\r\n";
get_http_status_code(301) ->
	"HTTP/1.1 301 Moved Permanently\r\n";
get_http_status_code(400) ->
	"HTTP/1.1 400 Bad Request\r\n";
get_http_status_code(401) ->
	"HTTP/1.1 401 Unauthorized\r\n";	
get_http_status_code(403) ->
	"HTTP/1.1 403 Forbidden\r\n";
get_http_status_code(404) ->
	"HTTP/1.1 404 Not Found\r\n";				
get_http_status_code(408) ->
	"HTTP/1.1 408 Request Timeout\r\n";			
get_http_status_code(500) ->
	"HTTP/1.1 500 Internal Server Error\r\n";
get_http_status_code(501) ->
	"HTTP/1.1 501 Not Implemented\r\n";				
% less common last
get_http_status_code(201) ->
	"HTTP/1.1 201 Created\r\n";
get_http_status_code(202) ->
	"HTTP/1.1 202 Accepted\r\n";
get_http_status_code(203) ->
	"HTTP/1.1 203 Non-Authoritative Information\r\n";
get_http_status_code(204) ->
	"HTTP/1.1 204 No Content\r\n";
get_http_status_code(205) ->
	"HTTP/1.1 205 Reset Content\r\n";
get_http_status_code(206) ->
	"HTTP/1.1 206 Partial Content\r\n";
get_http_status_code(300) ->
	"HTTP/1.1 300 Multiple Choices\r\n";
get_http_status_code(302) ->
	"HTTP/1.1 302 Found\r\n";
get_http_status_code(303) ->
	"HTTP/1.1 303 See Other\r\n";
get_http_status_code(304) ->
	"HTTP/1.1 304 Not Modified\r\n";
get_http_status_code(305) ->
	"HTTP/1.1 305 Use Proxy\r\n";
get_http_status_code(307) ->
	"HTTP/1.1 307 Temporary Redirect\r\n";
get_http_status_code(402) ->
	"HTTP/1.1 402 Payment Required\r\n";
get_http_status_code(405) ->
	"HTTP/1.1 405 Method Not Allowed\r\n";
get_http_status_code(406) ->
	"HTTP/1.1 406 Not Acceptable\r\n";
get_http_status_code(407) ->
	"HTTP/1.1 407 Proxy Authentication Required\r\n";
get_http_status_code(409) ->
	"HTTP/1.1 409 Conflict\r\n";
get_http_status_code(410) ->
	"HTTP/1.1 410 Gone\r\n";
get_http_status_code(411) ->
	"HTTP/1.1 411 Length Required\r\n";
get_http_status_code(412) ->
	"HTTP/1.1 412 Precondition Failed\r\n";
get_http_status_code(413) ->
	"HTTP/1.1 413 Request Entity Too Large\r\n";
get_http_status_code(414) ->
	"HTTP/1.1 414 Request-URI Too Long\r\n";
get_http_status_code(415) ->
	"HTTP/1.1 415 Unsupported Media Type\r\n";
get_http_status_code(416) ->
	"HTTP/1.1 416 Requested Range Not Satisfiable\r\n";
get_http_status_code(417) ->
	"HTTP/1.1 417 Expectation Failed\r\n";
get_http_status_code(502) ->
	"HTTP/1.1 502 Bad Gateway\r\n";
get_http_status_code(503) ->
	"HTTP/1.1 503 Service Unavailable\r\n";
get_http_status_code(504) ->
	"HTTP/1.1 504 Gateway Timeout\r\n";
get_http_status_code(505) ->
	"HTTP/1.1 505 HTTP Version Not Supported\r\n";
get_http_status_code(Other) ->
	lists:flatten(io_lib:format("HTTP/1.1 ~p \r\n", [Other])).

% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
