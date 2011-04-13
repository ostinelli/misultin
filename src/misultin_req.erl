% ==========================================================================================================
% MISULTIN - Request
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>,
%					  Bob Ippolito <bob@mochimedia.com> for Mochi Media, Inc.
% All rights reserved.
%
% Code portions from Bob Ippolito have been originally taken under MIT license from MOCHIWEB:
% <http://code.google.com/p/mochiweb/>
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
-module(misultin_req, [Req, SocketPid]).
-vsn("0.7").

% macros
-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
					(C >= $a andalso C =< $f) orelse
					(C >= $A andalso C =< $F))).
-define(FILE_READ_BUFFER, 64*1012).

% API
-export([raw/0]).
-export([ok/1, ok/2, ok/3, respond/1, respond/2, respond/3, respond/4, raw_headers_respond/1, raw_headers_respond/2, raw_headers_respond/3, raw_headers_respond/4]).
-export([options/1]).
-export([chunk/1, chunk/2, stream/1, stream/2, stream/3]).
-export([get/1, parse_qs/0, parse_post/0, file/1, file/2, file/3, resource/1]).

% includes
-include("../include/misultin.hrl").
-include_lib("kernel/include/file.hrl").


% ============================ \/ API ======================================================================

% Description: Returns raw request content.
raw() ->
	Req.

% Description: Get request info.
get(socket) ->
	Req#req.socket;
get(socket_mode) ->
	Req#req.socket_mode;
get(peer_addr) ->
	Req#req.peer_addr;
get(peer_port) ->
	Req#req.peer_port;
get(peer_cert) ->
	Req#req.peer_cert;
get(connection) ->
	Req#req.connection;
get(content_length) ->
	Req#req.content_length;
get(vsn) ->
	Req#req.vsn;
get(method) ->
	Req#req.method;
get(uri) ->
	Req#req.uri;
get(uri_unquoted) ->
	{_UriType, RawUri} = Req#req.uri,
	unquote(RawUri);
get(args) ->
	Req#req.args;
get(headers) ->
	Req#req.headers;
get(body) ->
	Req#req.body.

% Description: Formats a 200 response.
ok(Template) ->
	ok([], Template).
ok(Headers, Template) ->
	respond(200, Headers, Template).
ok(Headers, Template, Vars) ->
	respond(200, Headers, Template, Vars).

% Description: Formats a response.
respond(HttpCode) ->
	respond(HttpCode, [], []).
respond(HttpCode, Template) ->
	respond(HttpCode, [], Template).
respond(HttpCode, Headers, Template) ->
	SocketPid ! {HttpCode, Headers, Template}.
respond(HttpCode, Headers, Template, Vars) when is_list(Template) =:= true ->
	SocketPid ! {HttpCode, Headers, io_lib:format(Template, Vars)}.
	
% Description: Allow to add already formatted headers, untouched
raw_headers_respond(Body) ->
	raw_headers_respond(200, [], [], Body).
raw_headers_respond(HeadersStr, Body) ->
	raw_headers_respond(200, [], HeadersStr, Body).
raw_headers_respond(HttpCode, HeadersStr, Body) ->
	raw_headers_respond(HttpCode, [], HeadersStr, Body).
raw_headers_respond(HttpCode, Headers, HeadersStr, Body) ->
	SocketPid ! {HttpCode, {Headers, HeadersStr}, Body}.

% set advanced options valid for a single request
options(Options) when is_list(Options) ->
	% loop options and apply
	lists:foreach(fun({OptionTag, OptionVal}) -> options_set(OptionTag, OptionVal) end, Options).
% set to comet mode
options_set(comet, OptionVal) when OptionVal =:= true; OptionVal =:= false ->
	SocketPid ! {set_option, {comet, OptionVal}};
options_set(_OptionTag, _OptionVal)	->
	% ignore
	?LOG_DEBUG("ignoring advanced option ~p for request ~p", [{_OptionTag, _OptionVal}, Req]),
	ignore.

% Description: Chunked Transfer-Encoding.
chunk(head) ->
	chunk(head, []);
chunk(done) ->
	stream("0\r\n\r\n");
chunk(Template) ->
	stream([erlang:integer_to_list(iolist_size(Template), 16), "\r\n", Template, "\r\n"]).
chunk(head, Headers) ->
	% add Transfer-Encoding chunked header if needed
	Headers0 = case misultin_utility:header_get_value('Transfer-Encoding', Headers) of
		false -> [{'Transfer-Encoding', "chunked"} | Headers];
		_ -> Headers
	end,
	stream(head, Headers0);
chunk(Template, Vars) ->
	Data = io_lib:format(Template, Vars),
	stream([erlang:integer_to_list(length(Data), 16), "\r\n", Data, "\r\n"]).
	
% Description: Stream support.
stream(close) ->
	SocketPid ! stream_close;
stream(head) ->
	stream(head, 200, []);
stream({error, Reason}) ->
	SocketPid ! {stream_error, Reason};
stream(Data) ->
	catch SocketPid ! {stream_data, Data}.
stream(head, Headers) ->
	stream(head, 200, Headers);
stream(Template, Vars) when is_list(Template) =:= true ->
	catch SocketPid ! {stream_data, io_lib:format(Template, Vars)}.
stream(head, HttpCode, Headers) ->
	catch SocketPid ! {stream_head, HttpCode, Headers}.
	
% Description: Sends a file to the browser.
file(FilePath) ->
	file_send(FilePath, []).
% Description: Sends a file for download.	
file(attachment, FilePath) ->
	file(attachment, FilePath, []);
% Description: Sends a file to the browser with the given headers.
file(FilePath, Headers) ->
	file_send(FilePath, Headers).
% Description: Sends a file for download with the given headers.
file(attachment, FilePath, Headers) ->
	% get filename
	FileName = filename:basename(FilePath),
	% add Content-Disposition if needed
	Headers0 = case misultin_utility:header_get_value('Content-Disposition', Headers) of
		false -> [{'Content-Disposition', lists:flatten(io_lib:format("attachment; filename=~s", [FileName]))} | Headers];
		_ -> Headers
	end,
	file_send(FilePath, Headers0).

% Description: Parse QueryString
parse_qs() ->
	parse_qs(Req#req.args).

% Description: Parse Post
parse_post() ->
	% get header confirmation
	case misultin_utility:header_get_value('Content-Type', Req#req.headers) of
		false ->
			[];
		ContentType ->
			[Type|_CharSet] = string:tokens(ContentType, ";"),
			case Type of
				"application/x-www-form-urlencoded" ->
					parse_qs(Req#req.body);
				_Other ->
					[]
			end
	end.

% Description: Sets resource elements for restful services.
resource(Options) when is_list(Options) ->
	% clean uri
	{_UriType, RawUri} = Req#req.uri,
	Uri = lists:foldl(fun(Option, Acc) -> clean_uri(Option, Acc) end, RawUri, Options),
	% split
	string:tokens(Uri, "/").

% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% parse querystring & post
parse_qs(Binary) when is_binary(Binary) ->
	parse_qs(binary_to_list(Binary));
parse_qs(String) ->
	parse_qs(String, []).
parse_qs([], Acc) ->
	lists:reverse(Acc);
parse_qs(String, Acc) ->
	{Key, Rest} = parse_qs_key(String),
	{Value, Rest1} = parse_qs_value(Rest),
	parse_qs(Rest1, [{Key, Value} | Acc]).
parse_qs_key(String) ->
	parse_qs_key(String, []).
parse_qs_key([], Acc) ->
	{qs_revdecode(Acc), ""};
parse_qs_key([$= | Rest], Acc) ->
	{qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$; | _], Acc) ->
	{qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$& | _], Acc) ->
	{qs_revdecode(Acc), Rest};
parse_qs_key([C | Rest], Acc) ->
	parse_qs_key(Rest, [C | Acc]).
parse_qs_value(String) ->
	parse_qs_value(String, []).
parse_qs_value([], Acc) ->
	{qs_revdecode(Acc), ""};
parse_qs_value([$; | Rest], Acc) ->
	{qs_revdecode(Acc), Rest};
parse_qs_value([$& | Rest], Acc) ->
	{qs_revdecode(Acc), Rest};
parse_qs_value([C | Rest], Acc) ->
	parse_qs_value(Rest, [C | Acc]).

% revdecode
qs_revdecode(S) ->
	qs_revdecode(S, []).
qs_revdecode([], Acc) ->
	Acc;
qs_revdecode([$+ | Rest], Acc) ->
	qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
	qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
	qs_revdecode(Rest, [C | Acc]).

% unexdigit
unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

% unquote
unquote(Binary) when is_binary(Binary) ->
	unquote(binary_to_list(Binary));
unquote(String) ->
	qs_revdecode(lists:reverse(String)).

% Description: Clean URI.
clean_uri(lowercase, Uri) ->
	string:to_lower(Uri);
clean_uri(urldecode, Uri) ->
	unquote(Uri);	
% ignore unexisting option
clean_uri(_Unavailable, Uri) ->
	Uri.

% sending of a file
file_send(FilePath, Headers) ->
	% get file size
	case file:read_file_info(FilePath) of
		{ok, FileInfo} ->
			% get filesize
			FileSize = FileInfo#file_info.size,
			% do the gradual sending
			case file_open_and_send(FilePath, FileSize, Headers) of
				{error, Reason} ->
					stream({error, Reason});
				ok ->
					% sending successful
					ok
			end;
		{error, _Reason} ->
			% file not found or other errors
			stream({error, 404})
	end.
file_open_and_send(FilePath, FileSize, Headers) ->
	case file:open(FilePath, [read, binary]) of
		{error, Reason} ->
			{error, Reason};
		{ok, IoDevice} ->
			% send headers
			HeadersFull = [{'Content-Type', misultin_utility:get_content_type(FilePath)}, {'Content-Length', FileSize} | Headers],
			stream(head, HeadersFull),
			% read portions
			case file_read_and_send(IoDevice, 0) of 
				{error, Reason} ->
					file:close(IoDevice),
					{error, Reason};
				ok ->
					file:close(IoDevice),
					ok
			end
	end.
file_read_and_send(IoDevice, Position) ->
	% read buffer
	case file:pread(IoDevice, Position, ?FILE_READ_BUFFER) of
		{ok, Data} ->
			% file read, send
			stream(Data),
			% loop
			file_read_and_send(IoDevice, Position + ?FILE_READ_BUFFER);
		eof ->
			% finished
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
