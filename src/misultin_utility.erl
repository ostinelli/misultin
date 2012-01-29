% ==========================================================================================================
% MISULTIN - Various Utilities
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>,
%					  Bob Ippolito <bob@mochimedia.com> for Mochi Media, Inc.
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
-vsn("0.9").

% API
-export([get_http_status_code/2, get_http_status_message/1, get_content_type/1, get_key_value/2, header_get_value/2]).
-export([call/2, call/3, respond/2]).
-export([parse_qs/1, parse_qs/2, unquote/1, quote_plus/1, get_peer/2]).
-export([convert_ip_to_list/1]).
-export([hexstr/1, get_unix_timestamp/0, get_unix_timestamp/1]).
-export([sanitize_path_tokens/1]).

% macros
-define(INTERNAL_TIMEOUT, 30000).
-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), (
	(C >= $0 andalso C =< $9) orelse
	(C >= $a andalso C =< $f) orelse
	(C >= $A andalso C =< $F)
)).
-define(QS_SAFE(C), (
	(C >= $a andalso C =< $z) orelse
	(C >= $A andalso C =< $Z) orelse
	(C >= $0 andalso C =< $9) orelse
	(C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse C =:= $_)
)).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Returns a complete HTTP header
-spec get_http_status_code(HttpCode::non_neg_integer(), HttpVsn::http_version()) -> string().
get_http_status_code(HttpCode, {Maj, Min}) ->
	lists:flatten(io_lib:format("HTTP/~p.~p ~s\r\n", [Maj, Min, get_http_status_message(HttpCode)])).

% Returns the HTTP code description
-spec get_http_status_message(HttpStatus::non_neg_integer()) -> string().
get_http_status_message(200) ->
	"200 OK";
get_http_status_message(301) ->
	"301 Moved Permanently";
get_http_status_message(302) ->
	"302 Found";
get_http_status_message(400) ->
	"400 Bad Request";
get_http_status_message(401) ->
	"401 Unauthorized";	
get_http_status_message(403) ->
	"403 Forbidden";
get_http_status_message(404) ->
	"404 Not Found";
get_http_status_message(500) ->
	"500 Internal Server Error";
get_http_status_message(501) ->
	"501 Not Implemented";				
% less common last
get_http_status_message(100) ->
	"100 Continue";
get_http_status_message(101) ->
	"101 Switching Protocols";
get_http_status_message(102) ->
	"102 Processing";
get_http_status_message(103) ->
	"103 Checkpoint";
get_http_status_message(122) ->
	"122 Request-URI too long";
get_http_status_message(201) ->
	"201 Created";
get_http_status_message(202) ->
	"202 Accepted";
get_http_status_message(203) ->
	"203 Non-Authoritative Information";
get_http_status_message(204) ->
	"204 No Content";
get_http_status_message(205) ->
	"205 Reset Content";
get_http_status_message(206) ->
	"206 Partial Content";
get_http_status_message(207) ->
	"207 Multi-Status";
get_http_status_message(208) ->
	"208 Already Reported";
get_http_status_message(226) ->
	"226 IM Used";
get_http_status_message(300) ->
	"300 Multiple Choices";
get_http_status_message(303) ->
	"303 See Other";
get_http_status_message(304) ->
	"304 Not Modified";
get_http_status_message(305) ->
	"305 Use Proxy";
get_http_status_message(306) ->
	"306 Switch Proxy";
get_http_status_message(307) ->
	"307 Temporary Redirect";
get_http_status_message(308) ->
	"308 Resume Incomplete";
get_http_status_message(402) ->
	"402 Payment Required";
get_http_status_message(405) ->
	"405 Method Not Allowed";
get_http_status_message(406) ->
	"406 Not Acceptable";
get_http_status_message(407) ->
	"407 Proxy Authentication Required";				
get_http_status_message(408) ->
	"408 Request Timeout";
get_http_status_message(409) ->
	"409 Conflict";
get_http_status_message(410) ->
	"410 Gone";
get_http_status_message(411) ->
	"411 Length Required";
get_http_status_message(412) ->
	"412 Precondition Failed";
get_http_status_message(413) ->
	"413 Request Entity Too Large";
get_http_status_message(414) ->
	"414 Request-URI Too Long";
get_http_status_message(415) ->
	"415 Unsupported Media Type";
get_http_status_message(416) ->
	"416 Requested Range Not Satisfiable";
get_http_status_message(417) ->
	"417 Expectation Failed";
get_http_status_message(418) ->
	"418 I'm a teapot";
get_http_status_message(422) ->
	"422 Unprocessable Entity";
get_http_status_message(423) ->
	"423 Locked";
get_http_status_message(424) ->
	"424 Failed Dependency";
get_http_status_message(425) ->
	"425 Unordered Collection";
get_http_status_message(426) ->
	"426 Upgrade Required";
get_http_status_message(428) ->
	"428 Precondition Required";
get_http_status_message(429) ->
	"429 Too Many Requests";
get_http_status_message(431) ->
	"431 Request Header Fields Too Large";
get_http_status_message(444) ->
	"444 No Response";
get_http_status_message(449) ->
	"449 Retry With";
get_http_status_message(450) ->
	"450 Blocked by Windows Parental Controls";
get_http_status_message(499) ->
	"499 Client Closed Request";
get_http_status_message(502) ->
	"502 Bad Gateway";
get_http_status_message(503) ->
	"503 Service Unavailable";
get_http_status_message(504) ->
	"504 Gateway Timeout";
get_http_status_message(505) ->
	"505 HTTP Version Not Supported";
get_http_status_message(506) ->
	"506 Variant Also Negotiates";
get_http_status_message(507) ->
	"507 Insufficient Storage";
get_http_status_message(508) ->
	"508 Loop Detected";
get_http_status_message(509) ->
	"509 Bandwidth Limit Exceeded";
get_http_status_message(510) ->
	"510 Not Extended";
get_http_status_message(511) ->
	"511 Network Authentication Required";
get_http_status_message(598) ->
	"598 Network read timeout error";
get_http_status_message(599) ->
	"599 Network connect timeout error";
get_http_status_message(Other) ->
	lists:flatten(io_lib:format("~p \r\n", [Other])).

% get content type
-spec get_content_type(FileName::string()) -> string().
get_content_type(FileName) ->
	case filename:extension(FileName) of
		% most common first
		".doc" -> "application/msword";
		".exe" -> "application/octet-stream";
		".pdf" -> "application/pdf";
		".rtf" -> "application/rtf";
		".ppt" -> "application/vnd.ms-powerpoint";
		".tgz" -> "application/x-compressed";
		".tar" -> "application/x-tar";
		".zip" -> "application/zip";
		".mp3" -> "audio/mpeg";
		".wav" -> "audio/x-wav";
		".bmp" -> "image/bmp";
		".ram" -> "audio/x-pn-realaudio";
		".gif" -> "image/gif";
		".jpe" -> "image/jpeg";
		".jpeg" -> "image/jpeg";
		".jpg" -> "image/jpeg";
		".tif" -> "image/tiff";
		".tiff" -> "image/tiff";
		".png" -> "image/png";
		".htm" -> "text/html";
		".html" -> "text/html";
		".txt" -> "text/plain";
		".mp2" -> "video/mpeg";
		".mpa" -> "video/mpeg";
		".mpe" -> "video/mpeg";
		".mpeg" -> "video/mpeg";
		".mpg" -> "video/mpeg";
		".mov" -> "video/quicktime";
		".avi" -> "video/x-msvideo";
		".xml" -> "text/xml";
		% less common last
		".evy" -> "application/envoy";
		".fif" -> "application/fractals";
		".spl" -> "application/futuresplash";
		".hta" -> "application/hta";
		".acx" -> "application/internet-property-stream";
		".hqx" -> "application/mac-binhex40";
		".dot" -> "application/msword";
		".bin" -> "application/octet-stream";
		".class" -> "application/octet-stream";
		".dms" -> "application/octet-stream";
		".lha" -> "application/octet-stream";
		".lzh" -> "application/octet-stream";
		".oda" -> "application/oda";
		".axs" -> "application/olescript";
		".prf" -> "application/pics-rules";
		".p10" -> "application/pkcs10";
		".crl" -> "application/pkix-crl";
		".ai" -> "application/postscript";
		".eps" -> "application/postscript";
		".ps" -> "application/postscript";
		".setpay" -> "application/set-payment-initiation";
		".setreg" -> "application/set-registration-initiation";
		".xla" -> "application/vnd.ms-excel";
		".xlc" -> "application/vnd.ms-excel";
		".xlm" -> "application/vnd.ms-excel";
		".xls" -> "application/vnd.ms-excel";
		".xlt" -> "application/vnd.ms-excel";
		".xlw" -> "application/vnd.ms-excel";
		".msg" -> "application/vnd.ms-outlook";
		".sst" -> "application/vnd.ms-pkicertstore";
		".cat" -> "application/vnd.ms-pkiseccat";
		".stl" -> "application/vnd.ms-pkistl";
		".pot" -> "application/vnd.ms-powerpoint";
		".pps" -> "application/vnd.ms-powerpoint";
		".mpp" -> "application/vnd.ms-project";
		".wcm" -> "application/vnd.ms-works";
		".wdb" -> "application/vnd.ms-works";
		".wks" -> "application/vnd.ms-works";
		".wps" -> "application/vnd.ms-works";
		".hlp" -> "application/winhlp";
		".bcpio" -> "application/x-bcpio";
		".cdf" -> "application/x-cdf";
		".z" -> "application/x-compress";
		".cpio" -> "application/x-cpio";
		".csh" -> "application/x-csh";
		".dcr" -> "application/x-director";
		".dir" -> "application/x-director";
		".dxr" -> "application/x-director";
		".dvi" -> "application/x-dvi";
		".gtar" -> "application/x-gtar";
		".gz" -> "application/x-gzip";
		".hdf" -> "application/x-hdf";
		".ins" -> "application/x-internet-signup";
		".isp" -> "application/x-internet-signup";
		".iii" -> "application/x-iphone";
		".js" -> "application/x-javascript";
		".latex" -> "application/x-latex";
		".mdb" -> "application/x-msaccess";
		".crd" -> "application/x-mscardfile";
		".clp" -> "application/x-msclip";
		".dll" -> "application/x-msdownload";
		".m13" -> "application/x-msmediaview";
		".m14" -> "application/x-msmediaview";
		".mvb" -> "application/x-msmediaview";
		".wmf" -> "application/x-msmetafile";
		".mny" -> "application/x-msmoney";
		".pub" -> "application/x-mspublisher";
		".scd" -> "application/x-msschedule";
		".trm" -> "application/x-msterminal";
		".wri" -> "application/x-mswrite";
		".nc" -> "application/x-netcdf";
		".pma" -> "application/x-perfmon";
		".pmc" -> "application/x-perfmon";
		".pml" -> "application/x-perfmon";
		".pmr" -> "application/x-perfmon";
		".pmw" -> "application/x-perfmon";
		".p12" -> "application/x-pkcs12";
		".pfx" -> "application/x-pkcs12";
		".p7b" -> "application/x-pkcs7-certificates";
		".spc" -> "application/x-pkcs7-certificates";
		".p7r" -> "application/x-pkcs7-certreqresp";
		".p7c" -> "application/x-pkcs7-mime";
		".p7m" -> "application/x-pkcs7-mime";
		".p7s" -> "application/x-pkcs7-signature";
		".sh" -> "application/x-sh";
		".shar" -> "application/x-shar";
		".swf" -> "application/x-shockwave-flash";
		".sit" -> "application/x-stuffit";
		".sv4cpio" -> "application/x-sv4cpio";
		".sv4crc" -> "application/x-sv4crc";
		".tcl" -> "application/x-tcl";
		".tex" -> "application/x-tex";
		".texi" -> "application/x-texinfo";
		".texinfo" -> "application/x-texinfo";
		".roff" -> "application/x-troff";
		".t" -> "application/x-troff";
		".tr" -> "application/x-troff";
		".man" -> "application/x-troff-man";
		".me" -> "application/x-troff-me";
		".ms" -> "application/x-troff-ms";
		".ustar" -> "application/x-ustar";
		".src" -> "application/x-wais-source";
		".cer" -> "application/x-x509-ca-cert";
		".crt" -> "application/x-x509-ca-cert";
		".der" -> "application/x-x509-ca-cert";
		".pko" -> "application/ynd.ms-pkipko";
		".au" -> "audio/basic";
		".snd" -> "audio/basic";
		".mid" -> "audio/mid";
		".rmi" -> "audio/mid";
		".aif" -> "audio/x-aiff";
		".aifc" -> "audio/x-aiff";
		".aiff" -> "audio/x-aiff";
		".m3u" -> "audio/x-mpegurl";
		".ra" -> "audio/x-pn-realaudio";
		".cod" -> "image/cis-cod";
		".ief" -> "image/ief";
		".jfif" -> "image/pipeg";
		".svg" -> "image/svg+xml";
		".ras" -> "image/x-cmu-raster";
		".cmx" -> "image/x-cmx";
		".ico" -> "image/x-icon";
		".pnm" -> "image/x-portable-anymap";
		".pbm" -> "image/x-portable-bitmap";
		".pgm" -> "image/x-portable-graymap";
		".ppm" -> "image/x-portable-pixmap";
		".rgb" -> "image/x-rgb";
		".xbm" -> "image/x-xbitmap";
		".xpm" -> "image/x-xpixmap";
		".xwd" -> "image/x-xwindowdump";
		".mht" -> "message/rfc822";
		".mhtml" -> "message/rfc822";
		".nws" -> "message/rfc822";
		".css" -> "text/css";
		".323" -> "text/h323";
		".stm" -> "text/html";
		".uls" -> "text/iuls";
		".bas" -> "text/plain";
		".c" -> "text/plain";
		".h" -> "text/plain";
		".rtx" -> "text/richtext";
		".sct" -> "text/scriptlet";
		".tsv" -> "text/tab-separated-values";
		".htt" -> "text/webviewhtml";
		".htc" -> "text/x-component";
		".etx" -> "text/x-setext";
		".vcf" -> "text/x-vcard";
		".mpv2" -> "video/mpeg";
		".qt" -> "video/quicktime";
		".lsf" -> "video/x-la-asf";
		".lsx" -> "video/x-la-asf";
		".asf" -> "video/x-ms-asf";
		".asr" -> "video/x-ms-asf";
		".asx" -> "video/x-ms-asf";
		".movie" -> "video/x-sgi-movie";
		".flr" -> "x-world/x-vrml";
		".vrml" -> "x-world/x-vrml";
		".wrl" -> "x-world/x-vrml";
		".wrz" -> "x-world/x-vrml";
		".xaf" -> "x-world/x-vrml";
		".xof" -> "x-world/x-vrml";
		_ -> "application/octet-stream"
	end.

% faster than proplists:get_value
-spec get_key_value(Key::term(), List::[{term(), term()}]) -> undefined | term().
get_key_value(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		false-> undefined;
		{_K, Value}-> Value
	end.

% Find atom Tag in Headers, Headers being both atoms [for known headers] and strings. Comparison on string Header Tags is case insensitive.
-spec header_get_value(Tag::atom(), Headers::http_headers()) -> string() | false.
header_get_value(Tag, Headers) when is_atom(Tag) ->
	case lists:keyfind(Tag, 1, Headers) of
		false ->
			% header not found, test also conversion to string -> convert all string tags to lowercase (HTTP tags are case insensitive)
			F =	fun({HTag, HValue}) -> 
				case is_atom(HTag) of
					true -> {HTag, HValue};
					false -> {string:to_lower(HTag), HValue}
				end
			end,
			HeadersStr = lists:map(F, Headers),
			% test
			case lists:keyfind(string:to_lower(atom_to_list(Tag)), 1, HeadersStr) of
				false -> false;
				{_, Value} -> Value
			end;
		{_, Value} -> Value
	end.

% generic call function
-spec call(DestPid::pid(), Term::term()) -> {error, timeout} | term().
-spec call(DestPid::pid(), Term::term(), Timeout::non_neg_integer()) -> {error, timeout} | term().
call(DestPid, Term) ->
	call(DestPid, Term, ?INTERNAL_TIMEOUT).
call(DestPid, Term, Timeout) ->
	% send term
	DestPid ! {self(), Term},
	% wait for response
	receive
		{DestPid, Reply} -> Reply
	after Timeout ->
		{error, timeout}
	end.

% generic reply function
-spec respond(ReqPid::pid(), Term::term()) -> {Pid::pid(), Term::term()}.
respond(ReqPid, Term) ->
	ReqPid ! {self(), Term}.

%% @spec parse_qs(string() | binary()) -> [{Key, Value}]
%% @doc Parse a query string or application/x-www-form-urlencoded.
-spec parse_qs(string() | binary()) -> [{Key::string(), Value::string()}].
-spec parse_qs(Data::string() | binary(), Option::unicode | utf8) -> [{Key::string(), Value::string()}].
parse_qs(Data) ->
	parse_qs(Data, utf8).
parse_qs(Binary, Option) when is_binary(Binary) ->
	parse_qs(binary_to_list(Binary), Option);
parse_qs(String, Option) when Option =:= utf8; Option =:= unicode ->
	i_parse_qs(String, [], Option).
	
% unquote
-spec unquote(binary() | string()) -> string().
unquote(Binary) when is_binary(Binary) ->
	unquote(binary_to_list(Binary));
unquote(String) ->
	qs_revdecode(lists:reverse(String)).

%% @spec quote_plus(atom() | integer() | float() | string() | binary()) -> string()
%% @doc URL safe encoding of the given term.
-spec quote_plus(atom() | integer() | float() | string() | binary()) -> string().
quote_plus(Atom) when is_atom(Atom) ->
	quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
	quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
	quote_plus(binary_to_list(Binary));
quote_plus(String) ->
	quote_plus(String, []).


% convert an Ip tuple to string format
-spec convert_ip_to_list(Ip::term()) -> undefined | list().
convert_ip_to_list({A, B, C, D}) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
convert_ip_to_list({A, B, C, D, E, F, G, H}) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p.~p.~p.~p.~p", [A, B, C, D, E, F, G, H]));
convert_ip_to_list(_) ->
	undefined.

% convert binary to hex string
-spec hexstr(binary()) -> list().
hexstr(B) ->
	hexstr(B, []).
hexstr(<<>>, Acc) ->
	lists:reverse(Acc);
hexstr(<<N/integer, T/binary>>, Acc) ->
	hexstr(T, [hex(N rem 16), hex(N div 16) | Acc]).
hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

-spec get_unix_timestamp() -> TimeStamp::non_neg_integer().
-spec get_unix_timestamp({MegaSecs::non_neg_integer(), Secs::non_neg_integer(), _MicroSecs::non_neg_integer()}) -> TimeStamp::non_neg_integer().
get_unix_timestamp() -> get_unix_timestamp(erlang:now()).
get_unix_timestamp({MegaSecs, Secs, _MicroSecs}) -> MegaSecs * 1000000 + Secs.


% extract the peer address from the headers (in case of proxy specifying it in the address), or default to connection peer address
-spec get_peer(Headers::http_headers(), ConnectionPeerAddr::undefined | inet:ip_address()) -> {error, term()} | {ok, inet:ip_address()}.
get_peer(Headers, ConnectionPeerAddr) ->
	Host = case header_get_value('X-Real-Ip', Headers) of
		false ->
			case header_get_value('X-Forwarded-For', Headers) of
				false -> undefined;
				Hosts0 -> string:strip(lists:nth(1, string:tokens(Hosts0, ",")))
			end;
		Host0 -> Host0
	end,
	case Host of
		undefined ->
			{ok, ConnectionPeerAddr};
		_ ->
			case inet_parse:address(Host) of
				{error, _Reason} ->
					ConnectionPeerAddr;
				{ok, IpTuple} ->
					IpTuple
			end
	end.

% sanitize path tokens to avoid directory traversal attacks
-spec sanitize_path_tokens(Path::list(string())) -> invalid | list(string()).
sanitize_path_tokens(Path) ->
	i_sanitize_path_tokens(Path).

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% parse querystring & post
-spec i_parse_qs(String::string(), Acc::[{Key::string(), Value::string()}], Option::unicode | utf8) -> [{Key::string(), Value::string()}].
i_parse_qs([], Acc, _Option) ->
	lists:reverse(Acc);
i_parse_qs(String, Acc, Option) ->
	{Key, Rest} = parse_qs_key(String),
	{Value, Rest1} = parse_qs_value(Rest),
	case Option of
		unicode ->
			i_parse_qs(Rest1, [{unicode:characters_to_list(list_to_binary(Key)), unicode:characters_to_list(list_to_binary(Value))} | Acc], Option);
		_ ->
			i_parse_qs(Rest1, [{Key, Value} | Acc], Option)
	end.

% parse key & value
-spec parse_qs_key(String::string()) -> {Key::string(), Rest::string()}.
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
-spec qs_revdecode(string()) -> string().
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
-spec unhexdigit(char()) -> char().
-spec hexdigit(char()) -> char().
unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.
hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

% quote
-spec quote_plus(string(), string()) -> string().
quote_plus([], Acc) ->
	lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
	quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
	quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
	<<Hi:4, Lo:4>> = <<C>>,
	quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

-spec i_sanitize_path_tokens(Path::list(string())) -> invalid | list(string()).
i_sanitize_path_tokens(Path) ->
	% ensure no backslash \ character is included in path tokens
	F = fun(S) ->
		case string:str(S, "\\") of
			0 -> false;
			_ -> true
		end
	end,
	case lists:any(F, Path) of
		true ->
			% backslash found
			invalid;
		_ ->
			% proceed to check for sanity
			i_sanitize_path_tokens(lists:reverse(Path), 0, [])
	end.
i_sanitize_path_tokens([], RemCount, _Acc) when RemCount > 0 ->
	invalid;
i_sanitize_path_tokens([], _RemCount, Acc) ->
	Acc;
i_sanitize_path_tokens([".."|[]], _RemCount, _Acc) ->
	invalid;
i_sanitize_path_tokens([".."|T], RemCount, Acc) ->
	i_sanitize_path_tokens(T, RemCount + 1, Acc);
i_sanitize_path_tokens([_H|T], RemCount, Acc) when RemCount > 0 ->
	i_sanitize_path_tokens(T, RemCount - 1, Acc);
i_sanitize_path_tokens([H|T], RemCount, Acc) ->
	i_sanitize_path_tokens(T, RemCount, [H|Acc]).

% ============================ /\ INTERNAL FUNCTIONS =======================================================


% ============================ \/ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

i_sanitize_path_tokens_test() ->
	?assertEqual(["one", "two", "three"], i_sanitize_path_tokens(["one", "two", "three"])),
	?assertEqual(["one", "three"], i_sanitize_path_tokens(["one", "two", "..", "three"])),
	?assertEqual(["three"], i_sanitize_path_tokens(["one", "two", "..", "..", "three"])),
	?assertEqual(["three"], i_sanitize_path_tokens(["one", "two", "..", "..", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["one", "two", "..", "..", "..", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["one", "..", "..", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["..", "..", "..", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["..", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens([".."])),
	?assertEqual(invalid, i_sanitize_path_tokens(["..", "..", "one", "two", "three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["one", "two", "\\three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["one", "..", "\\three"])),
	?assertEqual(invalid, i_sanitize_path_tokens(["\\..\\one"])).
-endif.

% ============================ /\ TESTS ====================================================================
