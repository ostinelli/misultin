% ==========================================================================================================
% MISULTIN - Example: Comet - Long Polling Method
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
-module(misultin_comet_long_polling).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req, Port) end}]).

% stop misultin
stop() ->
	misultin:stop().

handle_http(Req, Port) ->
	% dispatch to rest
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req, Port).
	
% handle a GET on /
handle('GET', [], Req, Port) ->
	% output
	Req:ok([{"Content-Type", "text/html"}],
	["
	<html>
		<head>
			<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.5.1.min.js\"></script>
			<script type=\"text/javascript\">
				function misultinComet(){
					$.get('http://localhost:", erlang:integer_to_list(Port), "/comet', {}, function(response){
						toDiv(response);
						setTimeout('misultinComet()', 1000);
					});
				}
				function toDiv(content){
					$('#content').append(content + '<br>');
				}
				$(document).ready(function() {
					misultinComet();
				});
			</script>
		</head>
		<body>
			Long Polling example, please wait 10 seconds for incoming data.<br><br>
			<div id=\"content\"></div>
		</body>
	</html>	
	"]);
	
% handle a GET on /comet
handle('GET', ["comet"], Req, _Port) ->
	% set comet true, this will allow trapping client closing the connection
	Req:options([{comet, true}]),
	% simulate a long polling with timer
	timer:sleep(10000),
	Req:ok([{"Content-Type", "text/plain"}], ["Message received from Long Polling, next message in 10 seconds."]);

% handle the 404 page not found
handle(_, _, Req, _Port) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.").
