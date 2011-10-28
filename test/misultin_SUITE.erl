% ==========================================================================================================
% MISULTIN - Example: Chunk.
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, portions of websocket testing code from
%					  Loïc Hoguin <essen@dev-extend.eu>
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
-module(misultin_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").


% Note: run with ct_run -suite $PWD/misultin_SUITE -pa $PWD/ebin -logdir $PWD/test/results

% ============================ \/ COMMON_TEST CALLBACKS ====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: suite() -> Info
% Info = [tuple()]
% ----------------------------------------------------------------------------------------------------------
suite() ->
	[{timetrap,{seconds,30}}].

% ----------------------------------------------------------------------------------------------------------
% Function: init_per_suite(Config0) ->
%				Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
% Config0 = Config1 = [tuple()]
% Reason = term()
% ----------------------------------------------------------------------------------------------------------
init_per_suite(Config) ->
	inets:start(),
	[{port, 8080}|Config].	% this port and port + 1 will be used for tests, so by default 8080 and 8081

% ----------------------------------------------------------------------------------------------------------
% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
% Config0 = Config1 = [tuple()]
% ----------------------------------------------------------------------------------------------------------
end_per_suite(_Config) ->
	ok.

% ----------------------------------------------------------------------------------------------------------
% Function: init_per_group(GroupName, Config0) ->
%				Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
% GroupName = atom()
% Config0 = Config1 = [tuple()]
% Reason = term()
% ----------------------------------------------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
	Config.

% ----------------------------------------------------------------------------------------------------------
% Function: end_per_group(GroupName, Config0) ->
%				void() | {save_config,Config1}
% GroupName = atom()
% Config0 = Config1 = [tuple()]
% ----------------------------------------------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
	ok.

% ----------------------------------------------------------------------------------------------------------
% Function: init_per_testcase(TestCase, Config0) ->
%				Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
% TestCase = atom()
% Config0 = Config1 = [tuple()]
% Reason = term()
% ----------------------------------------------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
	Config.

% ----------------------------------------------------------------------------------------------------------
% Function: end_per_testcase(TestCase, Config0) ->
%				void() | {save_config,Config1} | {fail,Reason}
% TestCase = atom()
% Config0 = Config1 = [tuple()]
% Reason = term()
% ----------------------------------------------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
	ok.

% ----------------------------------------------------------------------------------------------------------
% Function: groups() -> [Group]
% Group = {GroupName,Properties,GroupsAndTestCases}
% GroupName = atom()
% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
% TestCase = atom()
% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%			   repeat_until_any_ok | repeat_until_any_fail
% N = integer() | forever
% ----------------------------------------------------------------------------------------------------------
groups() ->
	[].

% ----------------------------------------------------------------------------------------------------------
% Function: all() -> GroupsAndTestCases | {skip,Reason}
% GroupsAndTestCases = [{group,GroupName} | TestCase]
% GroupName = atom()
% TestCase = atom()
% Reason = term()
% ----------------------------------------------------------------------------------------------------------
all() -> 
	[
		http_basic,
		http_basic_ssl,
		http_nameless,
		http_two_servers,
		http_get_var,
		http_post_var,
		http_stream,
		http_chunk_send,
		http_compress,
		http_pipelining,
		http_cookie,
		http_websocket
	].

% ============================ /\ COMMON_TEST CALLBACKS ====================================================


% ============================ \/ TEST FUNCTIONS ===========================================================

% ----------------------------------------------------------------------------------------------------------
% Function: TestCase() -> Info
% Info = [tuple()]
% ----------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------
% Function: TestCase(Config0) ->
%				ok | exit() | {skip,Reason} | {comment,Comment} |
%				{save_config,Config1} | {skip_and_save,Reason,Config1}
% Config0 = Config1 = [tuple()]
% Reason = term()
% Comment = term()
% ----------------------------------------------------------------------------------------------------------

% ---------------------------- \/ BASIC HELLO WORLD --------------------------------------------------------
http_basic(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_basic_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_basic"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []),
	misultin:stop(),
	ok.
http_basic_handle(Req) ->
	Req:ok("http_basic").
% ---------------------------- /\ BASIC HELLO WORLD --------------------------------------------------------

% ---------------------------- \/ BASIC SSL HELLO WORLD ----------------------------------------------------
http_basic_ssl(Config) -> 
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_basic_ssl_handle(Req) end},
	{ssl, [
		{certfile, filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "test_certificate.pem"])},
		{keyfile, filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "test_privkey.pem"])},
		{password, "misultin"}
	]}]),
	{ok, {{_, 200, _}, _, "http_basic_ssl"}} = httpc:request(get, {"https://localhost:" ++ integer_to_list(Port), []}, [], []),
	misultin:stop(),
	ok.
http_basic_ssl_handle(Req) ->
	Req:ok("http_basic_ssl").
% ---------------------------- /\ BASIC SSL HELLO WORLD ----------------------------------------------------

% ---------------------------- \/ HELLO WORLD NAMELESS -----------------------------------------------------
http_nameless(Config) -> 
	Port = proplists:get_value(port, Config),
	{ok, ServerPid} = misultin:start_link([{port, Port}, {name, false}, {loop, fun(Req) -> http_nameless_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_nameless"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []),
	misultin:stop(ServerPid),
	ok.
http_nameless_handle(Req) ->
	Req:ok("http_nameless").
% ---------------------------- /\ HELLO WORLD NAMELESS -----------------------------------------------------

% ---------------------------- \/ TWO SERVERS REGISTERED ---------------------------------------------------
http_two_servers(Config) -> 
	Port = proplists:get_value(port, Config),
	Port2 = Port + 1,
	misultin:start_link([{port, Port}, {name, misultin1}, {loop, fun(Req) -> http_two_servers_handle(Req, misultin1) end}]),
	misultin:start_link([{port, Port2}, {name, misultin2}, {loop, fun(Req) -> http_two_servers_handle(Req, misultin2) end}]),
	{ok, {{_, 200, _}, _, "http_two_servers_misultin1"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []),
	{ok, {{_, 200, _}, _, "http_two_servers_misultin2"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port2), []}, [], []),
	misultin:stop(misultin1),
	misultin:stop(misultin2),
	ok.
http_two_servers_handle(Req, RegName) ->
	Req:ok(["http_two_servers_", atom_to_list(RegName)]).
% ---------------------------- /\ TWO SERVERS REGISTERED ---------------------------------------------------

% ---------------------------- \/ GET VARIABLE -------------------------------------------------------------
http_get_var(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_get_var_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_get_var"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port) ++ "/?value=http_get_var", []}, [], []),
	misultin:stop(),
	ok.
http_get_var_handle(Req) ->
	Args = Req:parse_qs(),
	Req:ok(Req:get_variable("value", Args)).
% ---------------------------- /\ GET VARIABLE -------------------------------------------------------------

% ---------------------------- \/ POST VARIABLE ------------------------------------------------------------
http_post_var(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_post_var_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_post_var"}} = httpc:request(post, {"http://localhost:" ++ integer_to_list(Port), [], "text/plain", <<"http_post_var">>}, [], []),
	misultin:stop(),
	ok.
http_post_var_handle(Req) ->
	%Args = Req:parse_post(),
	Req:ok(Req:get(body)).
% ---------------------------- /\ POST VARIABLE ------------------------------------------------------------

% ---------------------------- \/ STREAM -------------------------------------------------------------------
http_stream(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_stream_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_stream_test"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []),
	misultin:stop(),
	ok.
http_stream_handle(Req) ->
	Req:stream(head, [{"Content-Type", "text/plain"}]),
	Req:stream("http_"),
	Req:stream("stream_"),
	Req:stream("test"),
	Req:stream(close).
% ---------------------------- /\ STREAM -------------------------------------------------------------------

% ---------------------------- \/ CHUNK SEND ---------------------------------------------------------------
http_chunk_send(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_chunk_send_handle(Req) end}]),
	{ok, {{_, 200, _}, _, "http_chunked_test"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []),
	misultin:stop(),
	ok.
http_chunk_send_handle(Req) ->
	Req:chunk(head, [{"Content-Type", "text/plain"}]),
	Req:chunk("http_"),
	Req:chunk("chunked_"),
	Req:chunk("test"),
	Req:chunk(done).
% ---------------------------- /\ CHUNK SEND ---------------------------------------------------------------

% ---------------------------- \/ COMPRESS -----------------------------------------------------------------
http_compress(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {compress, true}, {loop, fun(Req) -> http_compress_handle(Req) end}]),
	Compressed = binary_to_list(zlib:gzip(<<"http_compress_test">>)),
	{ok, {{_, 200, _}, _, Compressed}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), [{"Accept-Encoding", "gzip"}]}, [], []),
	misultin:stop(),
	ok.
http_compress_handle(Req) ->
	Req:ok("http_compress_test").
% ---------------------------- /\ COMPRESS -----------------------------------------------------------------

% ---------------------------- \/ PIPELINING ---------------------------------------------------------------
http_pipelining(Config) ->
	Port = proplists:get_value(port, Config),
	NumRequests = 5,
	register(misultin_temp_pipelining_test, self()),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_pipelining_handle(Req) end}]),
	% send requests
	lists:foreach(fun(_El) ->
		{ok, _} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], [{sync, false}, {receiver, fun http_pipelining_async/1}])
	end, lists:seq(1, NumRequests)),
	http_pipelining_loop(NumRequests).
http_pipelining_loop(0) ->
	misultin:stop(),
	ok; 
http_pipelining_loop(N) ->
	receive
		{{_, 200, _}, _, <<"http_pipelining">>} ->			
			http_pipelining_loop(N - 1);
		Result ->			
			misultin:stop(),
			exit({no_match, Result})
	after 10000 ->					
		misultin:stop(),
		exit(timeout)
	end.
http_pipelining_handle(Req) ->
	timer:sleep(1000),	% sleep to ensure that the other requests get pipelined
	Req:ok("http_pipelining").
http_pipelining_async({_RequestId, Result}) ->
	misultin_temp_pipelining_test ! Result.
% ---------------------------- /\ PIPELINING ---------------------------------------------------------------

% ---------------------------- \/ COOKIES ------------------------------------------------------------------
http_cookie(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> http_cookie_handle_dispatch(Req) end}]),
	httpc:set_options([{cookies, enabled}]),
	% set cookie & read
	{ok, {{_, 200, _}, _, ""}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [], []), % set cookie
	Cookies = httpc:which_cookies(),
	{session_cookies, SessionCookies} =	 lists:keyfind(session_cookies, 1, Cookies),
	{http_cookie, _, _,"misultin_test_cookie", "misultin_test_cookie_value", _,	 _, _, _, _, _} = lists:keyfind("misultin_test_cookie", 4, SessionCookies),
	% send new request to check cookie
	{ok, {{_, 200, _}, _, "misultin_test_cookie_value"}} = httpc:request(get, {"http://localhost:" ++ integer_to_list(Port) ++ "/cookies", []}, [], []),	% get cookie
	misultin:stop(),
	ok.
http_cookie_handle_dispatch(Req) ->
	http_cookie_handle(Req:resource([lowercase, urldecode]), Req).
http_cookie_handle([], Req) ->
	Req:ok([Req:set_cookie("misultin_test_cookie", "misultin_test_cookie_value", [{max_age, 365*24*3600}])], "");
http_cookie_handle(["cookies"], Req) ->
	Cookies = Req:get_cookies(),
	Req:ok(Req:get_cookie_value("misultin_test_cookie", Cookies)).
% ---------------------------- /\ COOKIES ------------------------------------------------------------------

% ---------------------------- \/ WEBSOCKET ----------------------------------------------------------------
% note: testing draft-hixie 76 only
http_websocket(Config) ->
	Port = proplists:get_value(port, Config),
	misultin:start_link([{port, Port}, {loop, fun(_) -> ok end}, {ws_loop, fun(Ws) -> http_websocket_handle(Ws) end}]),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Upgrade: WebSocket\r\n"
		"Connection: Upgrade\r\n"
		"Host: localhost:", integer_to_list(Port), "\r\n"
		"Origin: http://localhost:", integer_to_list(Port), "\r\n"
		"Sec-Websocket-Key1: 4j	  20%  78u0 5y^	   /7 Lw56\r\n"
		"Sec-Websocket-Key2: 296324-q	  { -1S85  6\r\n"
		"\r\n", <<252,142,7,202,105,0,109,64>>
	]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 5000),
	{ok, {http_response, {1, 1}, 101, "WebSocket Protocol Handshake"}, Rest} = erlang:decode_packet(http, Handshake, []),
	[Headers, Body] = http_websocket_headers(erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "WebSocket"} = lists:keyfind('Upgrade', 1, Headers),
	WsSecLocation = "ws://localhost:" ++ integer_to_list(Port) ++ "/",
	WsSecOrigin = "http://localhost:" ++ integer_to_list(Port),
	{"sec-websocket-location", WsSecLocation} = lists:keyfind("sec-websocket-location", 1, Headers),
	{"sec-websocket-origin", WsSecOrigin} = lists:keyfind("sec-websocket-origin", 1, Headers),
	<<205,42,237,47,83,29,193,129,203,200,113,36,66,205,234,75>> = Body,
	ok = gen_tcp:send(Socket, <<0, "misultin_websocket_client_message", 255>>),
	{ok, <<0, "misultin_websocket_client_message", 255>>} = gen_tcp:recv(Socket, 0, 5000),
	ok = gen_tcp:send(Socket, <<255, 0>>),
	{ok, <<255, 0>>} = gen_tcp:recv(Socket, 0, 5000),
	{error, closed} = gen_tcp:recv(Socket, 0, 5000),
	misultin:stop(),
	ok.
http_websocket_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
http_websocket_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	http_websocket_headers(erlang:decode_packet(httph, Rest, []), [{F(Key), Value}|Acc]).
http_websocket_handle(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(Data),
			http_websocket_handle(Ws);
		_Ignore ->
			http_websocket_handle(Ws)
	after 5000 ->
		ok
	end.
% ---------------------------- /\ WEBSOCKET ----------------------------------------------------------------


% ============================ /\ TEST FUNCTIONS ===========================================================
