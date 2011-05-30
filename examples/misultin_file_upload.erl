% ==========================================================================================================
% MISULTIN - Example: File Upload.
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
-module(misultin_file_upload).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->
	% dispatch to rest
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).
	
% ---------------------------- \/ handle rest --------------------------------------------------------------

% handle a GET on /
handle('GET', [], Req) ->
	DestPath = get_destination_path(),
	Req:ok([{"Content-Type", "text/html"}], ["<html>
	<head>
		<title>Misultin File Upload</title>
	</head>
	<body>
		<p>
			Upload a File. This file will be saved in \"", DestPath, "\", please ensure that the appropriate writing permissions have been set.
		</p>
		<form action=\"/\" method=\"POST\" enctype=\"multipart/form-data\">
			<input type=\"file\" name=\"file\">
			<input type=\"submit\">
		</form>
	</body>
</html>"]);

% handle a POST on / -> file received
handle('POST', [], Req) ->
	case Req:parse_post() of
		[{_Tag, Attributes, FileData}] ->
			% build destination file path
			DestPath = get_destination_path(),
			FileName = misultin_utility:get_key_value("filename", Attributes),
			DestFile = filename:join(DestPath, FileName),
			% save file
			case file:write_file(DestFile, FileData) of
				ok ->
					Req:ok(["File has been successfully saved to \"", DestFile, "\"."]);
				{error, _Reason} ->
					Req:respond(500)
			end;
		_ ->
			Req:respond(500)
	end;
	
% handle the 404 page not found
handle(_, _, Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.").
	
% ---------------------------- /\ handle rest --------------------------------------------------------------

% gets the destination path
get_destination_path() ->
	filename:dirname(code:which(?MODULE)).
