% ==========================================================================================================
% MISULTIN - Example: Comet - iFrame Method
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, Example taken from
%                     <http://www.zeitoun.net/articles/comet_and_php/start>
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
-module(misultin_comet_iframe).
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
	["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
	<head>
		<title>Comet demo</title>
		<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
		<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/prototype/1.7.0.0/prototype.js\"></script>
	</head>
	<body>
		<div id=\"content\">The server time will be shown here in 5 seconds.</div>
		<script type=\"text/javascript\">
			var comet = {
				connection: false,
				iframediv: false,
				initialize: function() {
					if (navigator.appVersion.indexOf(\"MSIE\") != -1) {
						// For IE browsers
						comet.connection = new ActiveXObject(\"htmlfile\");
						comet.connection.open();
						comet.connection.write(\"<html>\");
						comet.connection.write(\"<script>document.domain = '\"+document.domain+\"'\");
						comet.connection.write(\"</html>\");
						comet.connection.close();
						comet.iframediv = comet.connection.createElement(\"div\");
						comet.connection.appendChild(comet.iframediv);
						comet.connection.parentWindow.comet = comet;
						comet.iframediv.innerHTML = \"<iframe id='comet_iframe' src='http://localhost:", erlang:integer_to_list(Port), "/comet'></iframe>\";
					} else if (navigator.appVersion.indexOf(\"KHTML\") != -1) {
						// for KHTML browsers
						comet.connection = document.createElement('iframe');
						comet.connection.setAttribute('id',     'comet_iframe');
						comet.connection.setAttribute('src',    'http://localhost:", erlang:integer_to_list(Port), "/comet');
						with (comet.connection.style) {
							position   = \"absolute\";
							left       = top   = \"-100px\";
							height     = width = \"1px\";
							visibility = \"hidden\";
						}
						document.body.appendChild(comet.connection);
					} else {
						// For other browser (Firefox...)
						comet.connection = document.createElement('iframe');
						comet.connection.setAttribute('id',     'comet_iframe');
						with (comet.connection.style) {
							left       = top   = \"-100px\";
							height     = width = \"1px\";
							visibility = \"hidden\";
							display    = 'none';
						}
						comet.iframediv = document.createElement('iframe');
						comet.iframediv.setAttribute('src', 'http://localhost:", erlang:integer_to_list(Port), "/comet');
						comet.connection.appendChild(comet.iframediv);
						document.body.appendChild(comet.connection);
					}
				},
				// this function will be called from /comet  
				printServerTime: function (time) {
					$('content').innerHTML = time;
				},
				onUnload: function() {
					if (comet.connection) {
						comet.connection = false; // release the iframe to prevent problems with IE when reloading the page
					}
				}
			}
			Event.observe(window, \"load\",   comet.initialize);
			Event.observe(window, \"unload\", comet.onUnload);
		</script>
	</body>
</html>
	"]);
	
% handle a GET on /comet
handle('GET', ["comet"], Req, _Port) ->
	% set comet true, this will allow trapping client closing the connection
	Req:options([{comet, true}]),
	% send headers
	Req:stream(head, [{"Content-Type", "text/html"}, {"Cache-Control", "no-cache, must-revalidate"}, {"Expires", "Mon, 26 Jul 1997 05:00:00 GMT"}]),
	% start the page
	Req:stream("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
	<head>
		<title>Comet php backend</title>
		<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
	</head>
	<body>
	<script type=\"text/javascript\">
		// KHTML browser don't share javascripts between iframes
		var is_khtml = navigator.appName.match(\"Konqueror\") || navigator.appVersion.match(\"KHTML\");
		if (is_khtml){
			var prototypejs = document.createElement('script');
			prototypejs.setAttribute('type','text/javascript');
			prototypejs.setAttribute('src','prototype.js');
			var head = document.getElementsByTagName('head');
			head[0].appendChild(prototypejs);
		}
		// load the comet object
		var comet = window.parent.comet;
	</script>
	"),
	% enter notification loop
	notify(Req);

% handle the 404 page not found
handle(_, _, Req, _Port) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.").

% notification loop
notify(Req) ->	
	% send a message every 5 seconds
	timer:sleep(5000),
	% get server local time
	{_Date, {Hour, Minutes, Seconds}} = erlang:localtime(),
	% send
	Req:stream(["
	<script type=\"text/javascript\">
		comet.printServerTime(\"Server current time is: ", erlang:integer_to_list(Hour), ":", erlang:integer_to_list(Minutes), ":", erlang:integer_to_list(Seconds), ", will be updated in 5 seconds.", "\");
	</script>
	"]),
	% loop
	notify(Req).


