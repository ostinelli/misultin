% ==========================================================================================================
% MISULTIN - Sessions Example.
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
-module(misultin_sessions_example).
-export([start/1, stop/0]).

% start misultin http server
start(Port) ->
	misultin:start_link([{port, Port}, {sessions_expire, 60}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->
	% get session info
	{SessionId, SessionState} = Req:session(),
	% check state
	Count = case SessionState of
		[] ->
			% no state set previously, init counter
			1;
		N ->
			% increment counter
			N + 1
	end,
	% save counter as session's state. a more complex state can easily be saved here, such as proplist()
	Req:save_session_state(SessionId, Count),
	% reply
	Req:ok([], "Session Id is: ~p, counter is: ~p", [SessionId, Count]).
