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
-export([start_link/1]).
-export([set_session_cookie/1, session/3, save_session_state/4]).

% internal
-export([expire_sessions/1]).

% macros
-define(TABLE_SESSIONS, misultin_table_sessions).
-define(RAND_BYTES_NUM, 128).
-define(SESSION_HEADER_NAME, "misultin_session").
-define(SESSION_EXPIRE_SEC, 15).		% 600 = ten minutes of inactivity.
-define(DATE_UPDATE_INTERVAL, 1000).	% compute table expired sessions every 1000ms = 1 sec

% records
-record(state, {
	table_sessions			= undefined :: undefined | ets:tid(),			% ETS table reference which holds the sessions id
	table_sessions_pid		= undefined :: undefined | ets:tid()			% ETS table reference which holds the sessions pid
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the server.
-spec start_link(term()) -> {ok, Pid::pid()} | {error, Error::term()}.
start_link(Options) when is_tuple(Options) ->
	gen_server:start_link(?MODULE, Options, []).

% return session cookie
set_session_cookie(SessionId) ->
	misultin_cookies:set_cookie(?SESSION_HEADER_NAME, SessionId, [{max_age, ?SESSION_EXPIRE_SEC}]).

% retrieve session info and start a session if necessary.
session(ServerRef, Cookies, Req) ->
	% extract session id
	SessionId = misultin_utility:get_key_value(?SESSION_HEADER_NAME, Cookies),
	% call
	gen_server:call(ServerRef, {session, SessionId, Req}).

% save a session state
save_session_state(ServerRef, SessionId, SessionState, Req) ->
	% call
	gen_server:call(ServerRef, {save_session_state, SessionId, SessionState, Req}).




% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init({}) ->
	process_flag(trap_exit, true),
	?LOG_DEBUG("starting session server with pid: ~p", [self()]),
	% create ets tables to save session data
	TableSessions= ets:new(?TABLE_SESSIONS, [set, public]),
	% start timer to expire sessions
	erlang:send_after(0, self(), expire_sessions),
	% return
	{ok, #state{table_sessions = TableSessions}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% start a session
handle_call({session, undefined, Req}, _From, #state{table_sessions = TableSessions} = State) ->
	?LOG_DEBUG("generate a new session",[]),
	SessionId = i_start_session(TableSessions, Req),
	{reply, {SessionId, []}, State};
handle_call({session, SessionId, Req}, _From, #state{table_sessions = TableSessions} = State) ->
	?LOG_DEBUG("starting or retrieving session ~p", [SessionId]),
	case ets:lookup(TableSessions, SessionId) of
		[] ->
			?LOG_DEBUG("session with id ~p could not be found, start new one", [SessionId]),
			NewSessionId = i_start_session(TableSessions, Req),
			{reply, {NewSessionId, []}, State};
		[{SessionId, VerifyInfo, _TSLastAccess, SessionState}] ->
			?LOG_DEBUG("session id ~p found in table with verify info: ~p, start validity check", [SessionId, VerifyInfo]),
			case is_session_valid(VerifyInfo, Req) of
				true ->
					?LOG_DEBUG("session id ~p is valid, return id and state", [SessionId]),
					% update session access data
					ets:insert(TableSessions, {SessionId, VerifyInfo, misultin_utility:get_unix_timestamp(), SessionState}),
					{reply, {SessionId, SessionState}, State};
				false ->
					?LOG_DEBUG("session is not valid, killing session and generate new",[]),
					i_remove_session(TableSessions, SessionId),
					NewSessionId = i_start_session(TableSessions, Req),
					{reply, {NewSessionId, []}, State}
			end
	end;

% save session state
handle_call({save_session_state, SessionId, SessionState, Req}, _From, #state{table_sessions = TableSessions} = State) ->
	?LOG_DEBUG("saving session ~p state ~p", [SessionId, SessionState]),
	case ets:lookup(TableSessions, SessionId) of
		[] ->
			?LOG_DEBUG("session with id ~p could not be found, could not save", [SessionId]),
			{reply, {error, invalid_session_id}, State};
		[{SessionId, VerifyInfo, _TSLastAccess, _OldSessionState}] ->
			?LOG_DEBUG("session id ~p found in table with verify info: ~p, start validity check", [SessionId, VerifyInfo]),
			case is_session_valid(VerifyInfo, Req) of
				true ->
					?LOG_DEBUG("session id ~p is valid, set new state ~p", [SessionId, SessionState]),
					ets:insert(TableSessions, {SessionId, VerifyInfo, misultin_utility:get_unix_timestamp(), SessionState}),
					{reply, ok, State};
				false ->
					?LOG_DEBUG("session is not valid, killing session",[]),
					i_remove_session(TableSessions, SessionId),
					{reply, {error, invalid_session_id}, State}
			end
	end;

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

% expire sessions
handle_info(expire_sessions, #state{table_sessions = TableSessions} = State) ->
	% avoid locking gen server
	Self = self(),
	spawn(fun() ->
		% expire sessions
		expire_sessions(TableSessions),
		% start date build timer
		erlang:send_after(?DATE_UPDATE_INTERVAL, Self, expire_sessions)
	end),
	{noreply, State};

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
	% delete ETS table
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

% start a new session
i_start_session(TableSessions, Req) ->
	% build verify info
	case get_ip_domain(Req#req.peer_addr) of
		{error, _Error} ->
			?LOG_DEBUG("could not start session: ~p", [_Error]),
			{error, could_not_start_session};
		VerifyDomain ->
			?LOG_DEBUG("build verify domain: ~p", [VerifyDomain]),
			% generate a session id
			SessionId = generate_session_id(),
			% insert into ETS
			VerifyInfo = {VerifyDomain},
			SessionState = [],
			TSLastAccess = misultin_utility:get_unix_timestamp(),
			ets:insert(TableSessions, {SessionId, VerifyInfo, TSLastAccess, SessionState}),
			?LOG_DEBUG("generated session id: ~p", [SessionId]),
			% return SessionId
			SessionId
	end.

% remove an existing session entry
i_remove_session(TableSessions, SessionId) ->
	ets:delete(TableSessions, SessionId).
	
% generate a session id string
generate_session_id() ->
	misultin_utility:hexstr(erlang:md5(crypto:rand_bytes(?RAND_BYTES_NUM))).

% get domain
get_ip_domain({A, B, C, _}) -> {A, B, C};	% ipv4
get_ip_domain({A, B, C, D, E, F, G, _}) -> {A, B, C, D, E, F, G};	% ipv6
get_ip_domain(_) -> {error, undefined_ip}.

% is session valid
is_session_valid({VerifyDomain}, Req) ->
	get_ip_domain(Req#req.peer_addr) =:= VerifyDomain.

% expire sessions
expire_sessions(TableSessions) ->
	TS = misultin_utility:get_unix_timestamp(),
	NumDeleted = ets:select_delete(TableSessions, [{{'_', '_', '$1', '_'}, [{'<', '$1', TS - ?SESSION_EXPIRE_SEC}], [true]}]),
	case NumDeleted > 0 of
		true -> ?LOG_DEBUG("removed ~p expired session(s)", [NumDeleted]);
		_ -> true
	end.


% ============================ /\ INTERNAL FUNCTIONS =======================================================
