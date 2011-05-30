% ==========================================================================================================
% MISULTIN - Example: Application based on Misultin - MAIN APPLICATION SUPERVISOR
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
-module(misultin_app_example_sup).
-behaviour(supervisor).

% API
-export([start_link/1]).

% supervisor callbacks
-export([init/1]).

% ============================ \/ API ======================================================================

% ----------------------------------------------------------------------------------------------------------
% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
% Description: Starts the supervisor
% ----------------------------------------------------------------------------------------------------------
start_link(Options) ->
	supervisor:start_link(?MODULE, [Options]).
	
% ============================ /\ API ======================================================================


% ============================ \/ SUPERVISOR CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok,  {SupFlags,  [ChildSpec]}} | ignore | {error, Reason}
% Description: Starts the supervisor
% ----------------------------------------------------------------------------------------------------------
init([Options]) ->
	% misultin specs
	MisultinSpecs = {misultin,
		{misultin, start_link, [Options]},
		permanent, infinity, supervisor, [misultin]
	},	
	% application gen server specs
	ServerSpecs = {misultin_app_example_server,
		{misultin_app_example_server, start_link, []},
		permanent, 60000, worker, [misultin_app_example_server]
	},
	% spawn
	{ok, {{one_for_all, 5, 30}, [MisultinSpecs, ServerSpecs]}}.

% ============================ /\ SUPERVISOR CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
