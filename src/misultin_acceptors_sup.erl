% ==========================================================================================================
% WIDEINVADERS - Acceptors Supervisor
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
% All rights reserved.
% ==========================================================================================================
-module(misultin_acceptors_sup).
-behaviour(supervisor).

% API
-export([start_link/1]).

% supervisor callbacks
-export([init/1]).

% includes
-include("../include/misultin.hrl").

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
init([[MainSupRef, Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, MaxConnections, SocketMode, CustomOpts]]) ->
	?LOG_DEBUG("starting listening ~p socket with options ~p on port ~p", [SocketMode, OptionsTcp, Port]),
	case misultin_socket:listen(Port, OptionsTcp, SocketMode) of
		{ok, ListenSocket} ->
			Acceptors = [
				{{acceptor, N}, {misultin_acceptor, start_link, [MainSupRef, ListenSocket, Port, RecvTimeout, MaxConnections, SocketMode, CustomOpts]},
				permanent, brutal_kill, worker, dynamic}
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
