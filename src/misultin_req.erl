% ==========================================================================================================
% MISULTIN - Request
%
% >-|-|-(°>
%
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
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
-module(misultin_req).
-vsn("0.8").

% macros
-define(FILE_READ_BUFFER, 64*1012).

% API
-export([ok/2, ok/3, ok/4, respond/2, respond/3, respond/4, respond/5]).
-export([raw_headers_respond/2, raw_headers_respond/3, raw_headers_respond/4, raw_headers_respond/5]).
-export([options/2]).
-export([chunk/2, chunk/3, stream/2, stream/3, stream/4]).
-export([raw/1, get/2, get_variable/3, get_cookies/1, get_cookie_value/3, set_cookie/3, set_cookie/4, delete_cookie/2]).
-export([parse_qs/1, parse_post/1, file/2, file/3, file/4, resource/2]).

% includes
-include("../include/misultin.hrl").
-include_lib("kernel/include/file.hrl").

% types
-type reqt() :: {misultin_req, #req{}, SocketPid::pid()}.


% ============================ \/ API ======================================================================

% Returns raw request content.
-spec raw(reqt()) -> #req{}.
raw({misultin_req, Req, _SocketPid}) ->
	Req.

% Get request info.
-spec get(ReqInfo::atom(), reqt()) -> term().
get(socket, {misultin_req, Req, _SocketPid}) ->
	Req#req.socket;
get(socket_mode, {misultin_req, Req, _SocketPid}) ->
	Req#req.socket_mode;
get(peer_addr, {misultin_req, #req{headers = Headers} = Req, _SocketPid}) ->
	Host = case misultin_utility:get_key_value("X-Real-Ip", Headers) of
		undefined ->
			case misultin_utility:get_key_value("X-Forwarded-For", Headers) of
				undefined -> undefined;
				Hosts0 -> string:strip(lists:nth(1, string:tokens(Hosts0, ",")))
			end;
		Host0 -> Host0
	end,
	case Host of
		undefined ->
			Req#req.peer_addr;
		_ ->
			case inet_parse:address(Host) of
				{error, _Reason} ->
					Req#req.peer_addr;
				{ok, IpTuple} ->
					IpTuple
			end
	end;
get(peer_port, {misultin_req, Req, _SocketPid}) ->
	Req#req.peer_port;
get(peer_cert, {misultin_req, Req, _SocketPid}) ->
	Req#req.peer_cert;
get(connection, {misultin_req, Req, _SocketPid}) ->
	Req#req.connection;
get(content_length, {misultin_req, Req, _SocketPid}) ->
	Req#req.content_length;
get(vsn, {misultin_req, Req, _SocketPid}) ->
	Req#req.vsn;
get(method, {misultin_req, Req, _SocketPid}) ->
	Req#req.method;
get(uri, {misultin_req, Req, _SocketPid}) ->
	Req#req.uri;
get(uri_unquoted, {misultin_req, Req, _SocketPid}) ->
	{_UriType, RawUri} = Req#req.uri,
	misultin_utility:unquote(RawUri);
get(args, {misultin_req, Req, _SocketPid}) ->
	Req#req.args;
get(headers, {misultin_req, Req, _SocketPid}) ->
	Req#req.headers;
get(body, {misultin_req, Req, _SocketPid}) ->
	Req#req.body.

% Get the value of a single variable
-spec get_variable(VarName::string(), Variables::gen_proplist(), reqt()) -> undefined | string().
get_variable(VarName, Variables, _ReqT) ->
	misultin_utility:get_key_value(VarName, Variables).

% Get all cookies.
-spec get_cookies(reqt()) -> gen_proplist().
get_cookies({misultin_req, #req{headers = Headers}, _SocketPid}) ->
	case misultin_utility:get_key_value('Cookie', Headers) of
		undefined -> [];
		CookieContent ->
			F = fun({Tag, Val}, Acc) ->
				[{misultin_utility:unquote(Tag), misultin_utility:unquote(Val)}|Acc]
			end,
			lists:foldl(F, [], misultin_cookies:parse_cookie(CookieContent))
	end.

% Get the value of a single cookie
-spec get_cookie_value(CookieTag::string(), Cookies::gen_proplist(), reqt()) -> undefined | string().
get_cookie_value(CookieTag, Cookies, _ReqT) ->
	misultin_utility:get_key_value(CookieTag, Cookies).

% set cookie
-spec set_cookie(Key::string(), Value::string(), reqt()) -> {http_header(), string()}.
-spec set_cookie(Key::string(), Value::string(), Options::cookies_options(), reqt()) -> {http_header(), string()}.
set_cookie(Key, Value, _ReqT) ->
	set_cookie(Key, Value, [], _ReqT).
set_cookie(Key, Value, Options, _ReqT) ->
	misultin_cookies:set_cookie(Key, Value, Options).

% delete cookie
-spec delete_cookie(Key::string(), reqt()) -> {http_header(), string()}.
delete_cookie(Key, _ReqT) ->
	misultin_cookies:delete_cookie(Key).

% Formats a 200 response.
-spec ok(Template::list() | binary() | iolist(), reqt()) -> term().
-spec ok(Headers::http_headers(), Template::list() | binary() | iolist(), reqt()) -> term().
-spec ok(Headers::http_headers(), Template::list(), Vars::[term()], reqt()) -> term().
ok(Template, ReqT) ->
	ok([], Template, ReqT).
ok(Headers, Template, ReqT) ->
	respond(200, Headers, Template, ReqT).
ok(Headers, Template, Vars, ReqT) ->
	respond(200, Headers, Template, Vars, ReqT).

% Formats a response.
-spec respond(HttpCode::non_neg_integer(), reqt()) -> term().
-spec respond(HttpCode::non_neg_integer(), Template::list() | binary() | iolist(), reqt()) -> term().
-spec respond(HttpCode::non_neg_integer(), Headers::http_headers(), Template::list() | binary() | iolist(), reqt()) -> term().
-spec respond(HttpCode::non_neg_integer(), Headers::http_headers(), Template::list(), Vars::[term()], reqt()) -> term().
respond(HttpCode, ReqT) ->
	respond(HttpCode, [], [], ReqT).
respond(HttpCode, Template, ReqT) ->
	respond(HttpCode, [], Template, ReqT).
respond(HttpCode, Headers, Template, {misultin_req, _Req, SocketPid}) ->
	SocketPid ! {response, HttpCode, Headers, Template}.
respond(HttpCode, Headers, Template, Vars, {misultin_req, _Req, SocketPid}) when is_list(Template) =:= true ->
	SocketPid ! {response, HttpCode, Headers, io_lib:format(Template, Vars)}.

% Allow to add already formatted headers, untouched
-spec raw_headers_respond(Body::binary(), reqt()) -> term().
-spec raw_headers_respond(HeadersStr::string(), Body::binary(), reqt()) -> term().
-spec raw_headers_respond(HttpCode::non_neg_integer(), HeadersStr::string(), Body::binary(), reqt()) -> term().
-spec raw_headers_respond(HttpCode::non_neg_integer(), Headers::http_headers(), HeadersStr::string(), Body::binary(), reqt()) -> term().
raw_headers_respond(Body, ReqT) ->
	raw_headers_respond(200, [], [], Body, ReqT).
raw_headers_respond(HeadersStr, Body, ReqT) ->
	raw_headers_respond(200, [], HeadersStr, Body, ReqT).
raw_headers_respond(HttpCode, HeadersStr, Body, ReqT) ->
	raw_headers_respond(HttpCode, [], HeadersStr, Body, ReqT).
raw_headers_respond(HttpCode, Headers, HeadersStr, Body, {misultin_req, _Req, SocketPid}) ->
	SocketPid ! {response, HttpCode, {Headers, HeadersStr}, Body}.

% set advanced options valid for a single request
-spec options(Options::gen_proplist(), reqt()) -> ok.
options(Options, ReqT) when is_list(Options) ->
	% loop options and apply
	lists:foreach(fun({OptionTag, OptionVal}) -> options_set(OptionTag, OptionVal, ReqT) end, Options).
% set to comet mode
-spec options_set(OptionName::atom(), OptionVal::term(), reqt()) -> term().
options_set(comet, OptionVal, {misultin_req, _Req, SocketPid}) when OptionVal =:= true; OptionVal =:= false ->
	SocketPid ! {set_option, {comet, OptionVal}};
options_set(_OptionTag, _OptionVal, {misultin_req, _Req, _SocketPid})	->
	% ignore
	?LOG_DEBUG("ignoring advanced option ~p for request ~p", [{_OptionTag, _OptionVal}, _Req]),
	ignore.

% Chunked Transfer-Encoding.
-spec chunk
	(head | done, reqt()) -> term();
	(Template::string() | binary() | iolist(), reqt()) -> term().
-spec chunk
	(head, Headers::http_headers(), reqt()) -> term();
	(Template::string(), Vars::[term()], reqt()) -> term().
chunk(head, ReqT) ->
	chunk(head, [], ReqT);
chunk(done, ReqT) ->
	stream("0\r\n\r\n", ReqT);
chunk(Template, ReqT) ->
	chunk_send(Template, ReqT).
chunk(head, Headers, ReqT) ->
	% add Transfer-Encoding chunked header if needed
	Headers0 = case misultin_utility:header_get_value('Transfer-Encoding', Headers) of
		false -> [{'Transfer-Encoding', "chunked"} | Headers];
		_ -> Headers
	end,
	stream(head, Headers0, ReqT);
chunk(Template, Vars, ReqT) ->
	chunk_send(io_lib:format(Template, Vars), ReqT).

-spec chunk_send(Data::string() | binary() | iolist(), reqt()) -> term().
chunk_send(Data, ReqT) ->
	stream([erlang:integer_to_list(erlang:iolist_size(Data), 16), "\r\n", Data, "\r\n"], ReqT).

% Stream support.
-spec stream
	(close | head | {error, Reason::term()}, reqt()) -> term();
	(Data::string() | binary() | iolist(), reqt()) -> term().
-spec stream
	(head, Headers::http_headers(), reqt()) -> term();
	(Template::string(), Vars::[term()], reqt()) -> term().
-spec stream
	(head, HttpCode::non_neg_integer(), Headers::http_headers(), reqt()) -> term().
stream(close, {misultin_req, _Req, SocketPid}) ->
	SocketPid ! stream_close;
stream(head, ReqT) ->
	stream(head, 200, [], ReqT);
stream({error, Reason}, {misultin_req, _Req, SocketPid}) ->
	SocketPid ! {stream_error, Reason};
stream(Data, {misultin_req, _Req, SocketPid}) ->
	catch SocketPid ! {stream_data, Data}.
stream(head, Headers, ReqT) ->
	stream(head, 200, Headers, ReqT);
stream(Template, Vars, {misultin_req, _Req, SocketPid}) when is_list(Template) =:= true ->
	catch SocketPid ! {stream_data, io_lib:format(Template, Vars)}.
stream(head, HttpCode, Headers, {misultin_req, _Req, SocketPid}) ->
	catch SocketPid ! {stream_head, HttpCode, Headers}.

% Sends a file to the browser.
-spec file
	(FilePath::string(), reqt()) -> term().
-spec file
	(attachment, FilePath::string(), reqt()) -> term();
	(FilePath::string(), Headers::http_headers(), reqt()) -> term().
-spec file
	(attachment, FilePath::string(), Headers::http_headers(), reqt()) -> term().
file(FilePath, ReqT) ->
	file_send(FilePath, [], ReqT).
% Sends a file for download.
file(attachment, FilePath, ReqT) ->
	file(attachment, FilePath, [], ReqT);
% Sends a file to the browser with the given headers.
file(FilePath, Headers, ReqT) ->
	file_send(FilePath, Headers, ReqT).
% Sends a file for download with the given headers.
file(attachment, FilePath, Headers, ReqT) ->
	% get filename
	FileName = filename:basename(FilePath),
	% add Content-Disposition if needed
	Headers0 = case misultin_utility:header_get_value('Content-Disposition', Headers) of
		false -> [{'Content-Disposition', lists:flatten(io_lib:format("attachment; filename=~s", [FileName]))} | Headers];
		_ -> Headers
	end,
	file_send(FilePath, Headers0, ReqT).

% Parse QueryString
-spec parse_qs(reqt()) -> string().
parse_qs({misultin_req, Req, _SocketPid}) ->
	misultin_utility:parse_qs(Req#req.args).

% Parse Post
-spec parse_post(reqt()) -> binary() | [{Id::string(), Attributes::gen_proplist(), Data::binary()}].
parse_post({misultin_req, Req, _SocketPid}) ->
	% get header confirmation
	case misultin_utility:header_get_value('Content-Type', Req#req.headers) of
		false ->
			[];
		ContentType ->
			[Type|Modificator] = string:tokens(ContentType, ";"),
			case Type of
			    "application/octet-stream" ->
			        Req#req.body;
				"application/x-www-form-urlencoded" ->
					misultin_utility:parse_qs(Req#req.body);
				"multipart/form-data" ->
					[Modificator0] = Modificator,
					"boundary=" ++ Boundary = string:strip(Modificator0),
					parse_multipart_form_data(Req#req.body, list_to_binary(Boundary));
				_Other ->
					[]
			end
	end.

% Sets resource elements for restful services.
-spec resource(Options::[term()], reqt()) -> [string()].
resource(Options, {misultin_req, Req, _SocketPid}) when is_list(Options) ->
	% clean uri
	{_UriType, RawUri} = Req#req.uri,
	Uri = lists:foldl(fun(Option, Acc) -> clean_uri(Option, Acc) end, RawUri, Options),
	% split
	string:tokens(Uri, "/").

% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Clean URI.
-spec clean_uri(Option::atom(), Uri::string()) -> string().
clean_uri(lowercase, Uri) ->
	string:to_lower(Uri);
clean_uri(urldecode, Uri) ->
	misultin_utility:unquote(Uri);
% ignore unexisting option
clean_uri(_Unavailable, Uri) ->
	Uri.

% sending of a file
-spec file_send(FilePath::string(), Headers::http_headers(), reqt()) -> term().
file_send(FilePath, Headers, ReqT) ->
	% get file size
	case file:read_file_info(FilePath) of
		{ok, FileInfo} ->
			% get filesize
			FileSize = FileInfo#file_info.size,
			% do the gradual sending
			case file_open_and_send(FilePath, FileSize, Headers, ReqT) of
				{error, Reason} ->
					stream({error, Reason}, ReqT);
				ok ->
					% sending successful
					ok
			end;
		{error, _Reason} ->
			% file not found or other errors
			stream({error, 404}, ReqT)
	end.
-spec file_open_and_send(FilePath::string(), FileSize::non_neg_integer(), Headers::http_headers(), reqt()) -> term().
file_open_and_send(FilePath, FileSize, Headers, ReqT) ->
	case file:open(FilePath, [read, binary]) of
		{error, Reason} ->
			{error, Reason};
		{ok, IoDevice} ->
			% send headers
			HeadersFull = [{'Content-Type', misultin_utility:get_content_type(FilePath)}, {'Content-Length', FileSize} | Headers],
			stream(head, HeadersFull, ReqT),
			% read portions
			case file_read_and_send(IoDevice, 0, ReqT) of
				{error, Reason} ->
					file:close(IoDevice),
					{error, Reason};
				ok ->
					file:close(IoDevice),
					ok
			end
	end.
-spec file_read_and_send(IoDevice::file:io_device(), Position::non_neg_integer(), reqt()) -> term().
file_read_and_send(IoDevice, Position, ReqT) ->
	% read buffer
	case file:pread(IoDevice, Position, ?FILE_READ_BUFFER) of
		{ok, Data} ->
			% file read, send
			stream(Data, ReqT),
			% loop
			file_read_and_send(IoDevice, Position + ?FILE_READ_BUFFER, ReqT);
		eof ->
			% finished
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

% parse multipart data
-spec parse_multipart_form_data(Body::binary(), Boundary::binary()) -> [{Id::string(), Attributes::gen_proplist(), Data::binary()}].
parse_multipart_form_data(Body, Boundary) ->
	[<<>> | Parts] = re:split(Body, <<"--", Boundary/binary>>),
	F = fun
		(<<"--\r\n">>, Data) -> Data;
		(Part, Data) ->
			case re:run(Part, "Content-Disposition:\\s*form-data;\\s*name=\"([^\"]+)\"(.*)\r\n\r\n(.+)\r\n$", [{capture, all_but_first, binary}, ungreedy, dotall]) of
				{match, [Key, Attributes, Value]} ->
					[{binary_to_list(Key), parse_attributes(Attributes), Value}|Data];
				_ ->
					?LOG_WARNING("Unknown part: ~p~n", [Part]),
					[]
			end
	end,
	lists:foldl(F, [], Parts).
-spec parse_attributes(Attributes::string()) -> gen_proplist().
parse_attributes(Attributes) ->
	case re:run(Attributes, "([^\"=;\s]+)=\"([^\"]+)\"", [{capture, all_but_first, list}, ungreedy, dotall, global]) of
		{match, Match} ->
			lists:reverse(lists:foldl(fun(X, Acc) -> [list_to_tuple(X)|Acc] end, [], Match));
		_ ->
			[]
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
