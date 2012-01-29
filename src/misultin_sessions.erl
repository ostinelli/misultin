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
-vsn("0.9").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1]).
-export([set_session_cookie/1, session/3, session/4, save_session_state/4]).

% macros
-define(TABLE_SESSIONS, misultin_table_sessions).
-define(RAND_BYTES_NUM, 128).
-define(SESSION_HEADER_NAME, "misultin_session").
-define(DEFAULT_SESSION_STATE, []).
-define(DATE_UPDATE_INTERVAL, 1000).	% compute table expired sessions every 1000ms = 1 sec

% records
-record(state, {
	table_sessions			= undefined :: undefined | ets:tid(),			% ETS table reference which holds the sessions
	table_date_ref			= undefined	:: undefined | ets:tid(),			% ETS table reference owned by misultin_server which holds the date
	sessions_expire_sec		= undefined :: undefined | non_neg_integer()	% number of seconds to expire cookie
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the server.
-spec start_link(term()) -> {ok, Pid::pid()} | {error, Error::term()}.
start_link(Options) when is_tuple(Options) ->
	gen_server:start_link(?MODULE, Options, []).

% return session cookie
-spec set_session_cookie(SessionId::string()) -> {http_header(), string()}.
set_session_cookie(SessionId) ->
	misultin_cookies:set_cookie(?SESSION_HEADER_NAME, SessionId, [{max_age, 365*24*3600}, {path, "/"}]).

% retrieve session info or start a session if necessary.
-spec session(ServerRef::pid(), Cookies::gen_proplist(), PeerAddr::inet:ip_address()) -> {SessionId::string(), SessionState::term()}.
-spec session(ServerRef::pid(), Cookies::gen_proplist(), PeerAddr::inet:ip_address(), CreateIfNonExistant::boolean()) -> {SessionId::string(), SessionState::term()} | {error, Reason::term()}.
session(ServerRef, Cookies, PeerAddr) ->
	session(ServerRef, Cookies, PeerAddr, true).
session(ServerRef, Cookies, PeerAddr, CreateIfNonExistant) ->
	% extract session id
	SessionId = misultin_utility:get_key_value(?SESSION_HEADER_NAME, Cookies),
	gen_server:call(ServerRef, {session, SessionId, PeerAddr, CreateIfNonExistant}).

% save a session state
-spec save_session_state(ServerRef::pid(), SessionId::string(), SessionState::term(), PeerAddr::inet:ip_address()) -> ok | {error, Reason::term()}.
save_session_state(ServerRef, SessionId, SessionState, PeerAddr) ->
	gen_server:call(ServerRef, {save_session_state, SessionId, SessionState, PeerAddr}).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init({MainSupRef, SessionsExpireSec}) ->
	?LOG_DEBUG("starting session server with pid: ~p", [self()]),
	% create ets tables to save session data
	TableSessions= ets:new(?TABLE_SESSIONS, [set, public]),
	% hack to retrieve misultin_server's TableDateRef
	erlang:send_after(0, self(), {get_tabledate_ref, MainSupRef}),
	% start timer to expire sessions
	erlang:send_after(?DATE_UPDATE_INTERVAL, self(), expire_sessions),
	% return
	{ok, #state{table_sessions = TableSessions, sessions_expire_sec = SessionsExpireSec}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% start a session
handle_call({session, undefined, PeerAddr, CreateIfNonExistant}, _From, #state{table_sessions = TableSessions, table_date_ref = TableDateRef} = State) ->
	case CreateIfNonExistant of
		true ->
			?LOG_DEBUG("generate a new session",[]),
			NewSessionId = i_start_session(TableSessions, TableDateRef, PeerAddr),
			{reply, {NewSessionId, ?DEFAULT_SESSION_STATE}, State};
		_ ->
			?LOG_DEBUG("do not generate a new session, report error",[]),
			{reply, {error, non_existent_session}, State}
	end;
			
handle_call({session, SessionId, PeerAddr, CreateIfNonExistant}, _From, #state{table_sessions = TableSessions, table_date_ref = TableDateRef} = State) ->
	?LOG_DEBUG("retrieving session ~p, starting new if non-existant is ~p", [SessionId, CreateIfNonExistant]),
	case ets:lookup(TableSessions, SessionId) of
		[] when CreateIfNonExistant =:= true ->
			?LOG_DEBUG("session with id ~p could not be found, start new one", [SessionId]),
			NewSessionId = i_start_session(TableSessions, TableDateRef, PeerAddr),
			{reply, {NewSessionId, ?DEFAULT_SESSION_STATE}, State};
		[] ->
			?LOG_DEBUG("session with id ~p could not be found, report error", [SessionId]),
			{reply, {error, non_existent_session}, State};
		[{SessionId, VerifyInfo, _TSLastAccess, SessionState}] ->
			?LOG_DEBUG("session id ~p found in table with verify info: ~p, start validity check", [SessionId, VerifyInfo]),
			case is_session_valid(VerifyInfo, PeerAddr) of
				true ->
					?LOG_DEBUG("session id ~p is valid, return id and state", [SessionId]),
					% update session access data
					ets:insert(TableSessions, {SessionId, VerifyInfo, misultin_server:get_timestamp(TableDateRef), SessionState}),
					{reply, {SessionId, SessionState}, State};
				false when CreateIfNonExistant =:= true ->
					?LOG_DEBUG("session is not valid, killing session and generate new",[]),
					i_remove_session(TableSessions, SessionId),
					NewSessionId = i_start_session(TableSessions, TableDateRef, PeerAddr),
					{reply, {NewSessionId, ?DEFAULT_SESSION_STATE}, State};
				false ->
					?LOG_DEBUG("session is not valid, killing session",[]),
					i_remove_session(TableSessions, SessionId),
					{reply, {error, non_existent_session}, State}
			end
	end;

% save session state
handle_call({save_session_state, SessionId, SessionState, PeerAddr}, _From, #state{table_sessions = TableSessions, table_date_ref = TableDateRef} = State) ->
	?LOG_DEBUG("saving session ~p state ~p", [SessionId, SessionState]),
	case ets:lookup(TableSessions, SessionId) of
		[] ->
			?LOG_DEBUG("session with id ~p could not be found, could not save", [SessionId]),
			{reply, {error, invalid_session_id}, State};
		[{SessionId, VerifyInfo, _TSLastAccess, _OldSessionState}] ->
			?LOG_DEBUG("session id ~p found in table with verify info: ~p, start validity check", [SessionId, VerifyInfo]),
			case is_session_valid(VerifyInfo, PeerAddr) of
				true ->
					?LOG_DEBUG("session id ~p is valid, set new state ~p", [SessionId, SessionState]),
					ets:insert(TableSessions, {SessionId, VerifyInfo, misultin_server:get_timestamp(TableDateRef), SessionState}),
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
handle_info(expire_sessions, #state{table_sessions = TableSessions, table_date_ref = TableDateRef, sessions_expire_sec = SessionsExpireSec} = State) ->
	% avoid locking gen server
	Self = self(),
	spawn(fun() ->
		% expire sessions
		expire_sessions(TableSessions, TableDateRef, SessionsExpireSec),
		% start date build timer
		erlang:send_after(?DATE_UPDATE_INTERVAL, Self, expire_sessions)
	end),
	{noreply, State};

% get tabledate ref just after init
handle_info({get_tabledate_ref, MainSupRef}, State) ->
	?LOG_DEBUG("getting misultin server's table_date reference",[]),
	% get pid of misultin server
	Childrens = supervisor:which_children(MainSupRef),
	case lists:keyfind(server, 1, Childrens) of
		{server, ServerRef, _, _} ->
			?LOG_DEBUG("got misultin server pid: ~p", [ServerRef]),
			% get rfc table ref
			TableDateRef = misultin_server:get_table_date_ref(ServerRef),
			?LOG_DEBUG("got misultin table date reference: ~p", [TableDateRef]),
			% save in state
			{noreply, State#state{table_date_ref = TableDateRef}};
		_ ->
			?LOG_ERROR("misultin session was unable to get server reference, shutting down", []),
			{stop, {error, could_not_get_serverref}, State}
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
-spec i_start_session(TableSessions::ets:tid(), TableDateRef::ets:tid(), PeerAddr::inet:ip_address()) -> SessionId::string() | {error, Reason::term()}.
i_start_session(TableSessions, TableDateRef, PeerAddr) ->
	% build verify info
	case get_ip_domain(PeerAddr) of
		{error, _Error} ->
			?LOG_DEBUG("could not start session: ~p", [_Error]),
			{error, could_not_start_session};
		VerifyDomain ->
			?LOG_DEBUG("build verify domain: ~p", [VerifyDomain]),
			% generate a session id
			SessionId = generate_session_id(),
			% insert into ETS
			VerifyInfo = {VerifyDomain},
			TSLastAccess = misultin_server:get_timestamp(TableDateRef),
			ets:insert(TableSessions, {SessionId, VerifyInfo, TSLastAccess, ?DEFAULT_SESSION_STATE}),
			?LOG_DEBUG("generated session id: ~p", [SessionId]),
			% return SessionId
			SessionId
	end.

% remove an existing session entry
-spec i_remove_session(TableSessions::ets:tid(), SessionId::string()) -> true.
i_remove_session(TableSessions, SessionId) ->
	ets:delete(TableSessions, SessionId).
	
% generate a session id string
-spec generate_session_id() -> SessionId::string().
generate_session_id() ->
	misultin_utility:hexstr(erlang:md5(crypto:rand_bytes(?RAND_BYTES_NUM))).

% get domain
-spec get_ip_domain(IpAddress::inet:ip_address()) -> {0..255, 0..255, 0..255} | {0..16#FFFF, 0..16#FFFF, 0..16#FFFF, 0..16#FFFF, 0..16#FFFF, 0..16#FFFF, 0..16#FFFF} | {error, Reason::term()}.
get_ip_domain({A, B, C, _}) -> {A, B, C};	% ipv4
get_ip_domain({A, B, C, D, E, F, G, _}) -> {A, B, C, D, E, F, G};	% ipv6
get_ip_domain(_) -> {error, undefined_ip}.

% is session valid
-spec is_session_valid(VerifyInfo::term(), PeerAddr::inet:ip_address()) -> boolean().
is_session_valid({VerifyDomain}, PeerAddr) ->
	get_ip_domain(PeerAddr) =:= VerifyDomain.

% expire sessions
-spec expire_sessions(TableSessions::ets:tid(), TableDateRef::ets:tid(), SessionsExpireSec::non_neg_integer()) -> true.
expire_sessions(TableSessions, TableDateRef, SessionsExpireSec) ->
	TS = misultin_server:get_timestamp(TableDateRef),
	NumDeleted = ets:select_delete(TableSessions, [{{'_', '_', '$1', '_'}, [{'<', '$1', TS - SessionsExpireSec + 1}], [true]}]),
	case NumDeleted > 0 of
		true -> ?LOG_DEBUG("removed ~p expired session(s)", [NumDeleted]), true;
		_ -> true
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
