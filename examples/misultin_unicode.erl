% ==========================================================================================================
% MISULTIN - Example: Unicode strings.
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
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
-module(misultin_unicode).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
	misultin:stop().

% callback function called on incoming http request
handle_http(Req) ->
	% dispatch to rest
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

% handle a GET on /
handle('GET', [], Req) ->
	Req:ok([{"Content-Type", "text/html"}], ["<html>
<head>
	<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">
</head>
<body>
	<form action=\"/\" method=\"post\">
		<input type=\"hidden\" name=\"data\" value=\"こんにちは\">
		<input type=\"submit\" value=\"GO!\">
	</form>
</body>
</html>"]);

% handle POST submit on /
handle('POST', [], Req) ->
	Args = Req:parse_post(unicode),
	case Req:get_variable("data", Args) of
		undefined ->
			Req:ok("No data value submitted.");
		Value ->
			% we have specified an unicode encoding, so ensure we convert the list back to binary before sending it down the socket
			ValueBin = unicode:characters_to_binary(Value),
			Req:ok([{"Content-Type", "text/html"}], ["<html>
<head>
	<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">
</head>
<body>
	<p>Unicode string is: \"", ValueBin, "\", and its length is ", integer_to_list(length(Value)), "
</body>
</html>"])
	end;
	
% handle the 404 page not found
handle(_, _, Req) ->
	Req:ok("Page not found.").

