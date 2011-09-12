% ==========================================================================================================
% MISULTIN - Default Sessions implementation.
%
% >-|-|-(°>
%
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
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
-module(misultin_sessions).
-behaviour(gen_server).
-vsn("0.8.1-dev").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1, start_session/2]).

% internal
-export([session_loop/3]).

% macros
-define(TABLE_SESSIONS, misultin_table_sessions).
-define(RAND_BYTES_NUM, 128).

% records
-record(state, {
	table_sessions			= undefined :: undefined | ets:tid()			% ETS table reference which holds the sessions
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the server.
-spec start_link(term()) -> {ok, Pid::pid()} | {error, Error::term()}.
start_link(Options) when is_tuple(Options) ->
	gen_server:start_link(?MODULE, Options, []).

% start a session
start_session(ServerRef, Req) ->
	gen_server:call(ServerRef, {start_session, Req}).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init({}) ->
	process_flag(trap_exit, true),
	?LOG_DEBUG("starting session server with pid: ~p", [self()]),
	% create ets tables to save open connections and websockets
	TableSessions = ets:new(?TABLE_SESSIONS, [set, public]),
	% return
	{ok, #state{table_sessions = TableSessions}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% start a session
handle_call({start_session, Req}, _From, #state{table_sessions = TableSessions} = State) ->
	?LOG_DEBUG("received start session request",[]),
	% generate a session id
	SessionId = generate_session_id(),
	% start a process
	spawn_link(fun() ->
		% add to ets table
		ets:insert(TableSessions, {SessionId, self(), Req#req.peer_addr, []}),
		% loop
		session_loop(SessionId, TableSessions, [])
	end),
	?LOG_DEBUG("generated new session process with id: ~p", [SessionId]),
	{reply, SessionId, State};

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	?LOG_WARNING("received unknown call message: ~p", [_Request]),
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?LOG_WARNING("received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle session process exits and crashes
handle_info({'EXIT', _SessionPid, normal}, State) -> {noreply, State};	% normal exiting of a session process
handle_info({'EXIT', SessionPid, _Reason}, #state{table_sessions = TableSessions} = State) ->
	case ets:member(TableSessions, SessionPid) of
		true ->
			?LOG_ERROR("session process ~p has died with reason: ~p, removing from session table", [SessionPid, _Reason]),
			ets:delete(TableSessions, SessionPid),
			{noreply, State};
		false ->
			?LOG_WARNING("received info on a process ~p crash which is not a session process, with reason: ~p", [SessionPid, _Reason]),
			{noreply, State}
	end;

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	?LOG_WARNING("received unknown info message: ~p", [_Info]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, #state{table_sessions = TableSessions}) ->
	?LOG_DEBUG("shutting down session server with Pid ~p with reason: ~p", [self(), _Reason]),
	% delete ETS tables
	?LOG_DEBUG("removing ets table",[]),
	ets:delete(TableSessions),
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% generate a session id string
generate_session_id() ->
	misultin_utility:hexstr(erlang:md5(crypto:rand_bytes(?RAND_BYTES_NUM))).


% session process
session_loop(SessionId, TableSessions, Variables) ->
	receive
		
		
		
		_Unknown ->
			?LOG_DEBUG("received unknown message ~p, ignoring", [_Unknown]),
			session_loop(SessionId, TableSessions, Variables)
	after 15000 ->
		?LOG_DEBUG("session id ~p has expired, shutting down session process and removing it from ets table", [SessionId]),
		ets:delete(TableSessions, SessionId)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
