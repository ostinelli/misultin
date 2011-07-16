% ==========================================================================================================
% MISULTIN - Various Utilities
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Emad El-Haraty <emad@mochimedia.com> for Mochi Media Inc.,
%					  Roberto Ostinelli <roberto@ostinelli.net>.
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
-module(misultin_cookies).
-vsn("0.8").

% API
-export([set_cookie/3, set_cookie/2, delete_cookie/1, parse_cookie/1]).

% Macros
-define(QUOTE, $\").
-define(IS_WHITESPACE(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C), (
	C < 32 orelse C =:= $\s orelse C =:= $\t orelse
	C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
	C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
	C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
	C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
	C =:= ${ orelse C =:= $}
)).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

%% @type proplist() = [{Key::string(), Value::string()}].
%% @type header() = {Name::string(), Value::string()}.
%% @type int_seconds() = integer().

%% @spec set_cookie(Key::string(), Value::string()) -> header()
%% @doc Short-hand for <code>cookie(Key, Value, [])</code>.
-spec set_cookie(Key::string(), Value::string()) -> {http_header(), string()}.
-spec set_cookie(Key::string(), Value::string(), Options::cookies_options()) -> {http_header(), string()}.
set_cookie(Key, Value) ->
	set_cookie(Key, Value, []).
set_cookie(Key, Value, Options) ->
	cookie(Key, misultin_utility:quote_plus(Value), Options).
	
%% Delete a cookie
-spec delete_cookie(Key::string()) -> {http_header(), string()}.
delete_cookie(Key) ->
	set_cookie(Key, "", [{max_age, -3600}]).

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
-spec parse_cookie(Cookie::string()) -> gen_proplist().
parse_cookie("") ->
	[];
parse_cookie(Cookie) ->
	parse_cookie(Cookie, []).

% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% SET COOKIE

%% @spec cookie(Key::string(), Value::string(), Options::[Option]) -> header()
%% where Option = {max_age, int_seconds()} | {local_time, {date(), time()}}
%%				  | {domain, string()} | {path, string()}
%%				  | {secure, true | false} | {http_only, true | false}
%%
%% @doc Generate a Set-Cookie header field tuple.
-spec cookie(Key::string(), Value::string(), Options::cookies_options()) -> {http_header(), string()}.
cookie(Key, Value, Options) ->
	Cookie = [any_to_list(Key), "=", quote(Value), "; Version=1"],
	%% Set-Cookie:
	%%	  Comment, Domain, Max-Age, Path, Secure, Version
	%% Set-Cookie2:
	%%	  Comment, CommentURL, Discard, Domain, Max-Age, Path, Port, Secure,
	%%	  Version
	ExpiresPart = case misultin_utility:get_key_value(max_age, Options) of
		undefined -> "";
		RawAge ->
			When = case misultin_utility:get_key_value(local_time, Options) of
				undefined -> calendar:local_time();
				LocalTime -> LocalTime
			end,
			Age = case RawAge < 0 of
				true -> 0;
				false -> RawAge
			end,
			["; Expires=", age_to_cookie_date(Age, When), "; Max-Age=", quote(Age)]
	end,
	SecurePart = case misultin_utility:get_key_value(secure, Options) of
		true -> "; Secure";
		_ -> ""
	end,
	DomainPart = case misultin_utility:get_key_value(domain, Options) of
		undefined -> "";
		Domain -> ["; Domain=", quote(Domain)]
	end,
	PathPart = case misultin_utility:get_key_value(path, Options) of
		undefined -> "";
		Path -> ["; Path=", quote(Path)]
	end,
	HttpOnlyPart = case misultin_utility:get_key_value(http_only, Options) of
		true -> "; HttpOnly";
		_ -> ""
	end,
	CookieParts = [Cookie, ExpiresPart, SecurePart, DomainPart, PathPart, HttpOnlyPart],
	{"Set-Cookie", lists:flatten(CookieParts)}.

%% Every major browser incorrectly handles quoted strings in a
%% different and (worse) incompatible manner.  Instead of wasting time
%% writing redundant code for each browser, we restrict cookies to
%% only contain characters that browsers handle compatibly.
%%
%% By replacing the definition of quote with this, we generate
%% RFC-compliant cookies:
%%
%%	   quote(V) ->
%%		   Fun = fun(?QUOTE, Acc) -> [$\\, ?QUOTE | Acc];
%%					(Ch, Acc) -> [Ch | Acc]
%%				 end,
%%		   [?QUOTE | lists:foldr(Fun, [?QUOTE], V)].

%% Convert to a string and raise an error if quoting is required.
-spec quote(Unquoted::term()) -> Quoted::string().
quote(V0) ->
	V = any_to_list(V0),
	lists:all(fun(Ch) -> Ch =:= $/ orelse not ?IS_SEPARATOR(Ch) end, V)
		orelse erlang:error({cookie_quoting_required, V}),
	V.

%% Return a date in the form of: Wdy, DD-Mon-YYYY HH:MM:SS GMT
%% See also: rfc2109: 10.1.2
-spec rfc2109_cookie_expires_date(LocalTime::date_tuple()) -> string().
rfc2109_cookie_expires_date(LocalTime) ->
	{{YYYY,MM,DD},{Hour,Min,Sec}} = case calendar:local_time_to_universal_time_dst(LocalTime) of
		[Gmt]	-> Gmt;
		[_,Gmt] -> Gmt
	end,
	DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
	lists:flatten(
		io_lib:format("~s, ~2.2.0w-~3.s-~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
		[httpd_util:day(DayNumber),DD,httpd_util:month(MM),YYYY,Hour,Min,Sec])
	).

-spec age_to_cookie_date(Age::integer(), LocalTime::date_tuple()) -> string().
age_to_cookie_date(Age, LocalTime) ->
	rfc2109_cookie_expires_date(add_seconds(Age, LocalTime)).
-spec add_seconds(Secs::integer(), LocalTime::date_tuple()) -> date_tuple().
add_seconds(Secs, LocalTime) ->
	Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
	calendar:gregorian_seconds_to_datetime(Greg + Secs).

% PARSE COOKIE
-spec parse_cookie(Cookie::string(), Acc::string()) -> gen_proplist().
parse_cookie([], Acc) ->
	lists:reverse(Acc);
parse_cookie(String, Acc) ->
	{{Token, Value}, Rest} = read_pair(String),
	Acc1 = case Token of
		"" -> Acc;
		"$" ++ _ -> Acc;
		_ -> [{Token, Value} | Acc]
	end,
	parse_cookie(Rest, Acc1).

read_pair(String) ->
	{Token, Rest} = read_token(skip_whitespace(String)),
	{Value, Rest1} = read_value(skip_whitespace(Rest)),
	{{Token, Value}, skip_past_separator(Rest1)}.

read_value([$= | Value]) ->
	Value1 = skip_whitespace(Value),
	case Value1 of
		[?QUOTE | _] ->
			read_quoted(Value1);
		_ ->
			read_token(Value1)
	end;
read_value(String) ->
	{"", String}.

read_quoted([?QUOTE | String]) ->
	read_quoted(String, []).

read_quoted([], Acc) ->
	{lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
	{lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) ->
	read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
	read_quoted(Rest, [C | Acc]).

skip_whitespace(String) ->
	F = fun (C) -> ?IS_WHITESPACE(C) end,
	lists:dropwhile(F, String).

read_token(String) ->
	F = fun (C) -> not ?IS_SEPARATOR(C) end,
	lists:splitwith(F, String).

skip_past_separator([]) ->
	[];
skip_past_separator([$; | Rest]) ->
	Rest;
skip_past_separator([$, | Rest]) ->
	Rest;
skip_past_separator([_ | Rest]) ->
	skip_past_separator(Rest).

any_to_list(V) when is_list(V) ->
	V;
any_to_list(V) when is_atom(V) ->
	atom_to_list(V);
any_to_list(V) when is_binary(V) ->
	binary_to_list(V);
any_to_list(V) when is_integer(V) ->
	integer_to_list(V).

% ============================ /\ INTERNAL FUNCTIONS =======================================================

