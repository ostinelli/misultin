% ==========================================================================================================
% MISULTIN - Acceptors Supervisor
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
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
-module(misultin_acceptors_sup).
-behaviour(supervisor).
-vsn("0.9").

% API
-export([start_link/7]).

% supervisor callbacks
-export([init/1]).

% includes
-include("../include/misultin.hrl").

% ============================ \/ API ======================================================================

% ----------------------------------------------------------------------------------------------------------
% Starts the supervisor
% ----------------------------------------------------------------------------------------------------------
-spec start_link(
	MainSupRef::pid(),
	Port::non_neg_integer(),
	OptionsTcp::[misultin_option_tcp()],
	AcceptorsPoolsize::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{}) -> {ok, Pid::pid()}.
start_link(MainSupRef, Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, SocketMode, CustomOpts) ->
	supervisor:start_link(?MODULE, {MainSupRef, Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, SocketMode, CustomOpts}).
	
% ============================ /\ API ======================================================================


% ============================ \/ SUPERVISOR CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok,  {SupFlags,  [ChildSpec]}} | ignore | {error, Reason}
% Description: Starts the supervisor
% ----------------------------------------------------------------------------------------------------------
-spec init({
	MainSupRef::pid(),
	Port::non_neg_integer(),
	OptionsTcp::[misultin_option_tcp()],
	AcceptorsPoolsize::non_neg_integer(),
	RecvTimeout::non_neg_integer(),
	SocketMode::socketmode(),
	CustomOpts::#custom_opts{} }) -> {ok, term()} | {error, Reason::term()}.
init({MainSupRef, Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, SocketMode, CustomOpts}) ->
	?LOG_DEBUG("starting listening ~p socket with options ~p on port ~p", [SocketMode, OptionsTcp, Port]),
	case misultin_socket:listen(Port, OptionsTcp, SocketMode) of
		{ok, ListenSocket} ->
			Acceptors = [
				{{acceptor, N}, {misultin_acceptor, start_link, [MainSupRef, ListenSocket, Port, RecvTimeout, SocketMode, CustomOpts]}, permanent, brutal_kill, worker, [misultin_acceptor]}
				|| N <- lists:seq(1, AcceptorsPoolsize)
			],
			{ok, {{one_for_one, 5, 10}, Acceptors}};
		{error, Reason} ->
			% error
			{error, Reason}
	end.

% ============================ /\ SUPERVISOR CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
