% ==========================================================================================================
% MISULTIN - Main Server
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
-module(misultin_server).
-behaviour(gen_server).
-vsn("dev-sup-0.8").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1]).
-export([http_pid_ref_add/2, http_pid_ref_remove/3, ws_pid_ref_add/2, ws_pid_ref_remove/2]).

% macros
-define(TABLE_PIDS_HTTP, pids_http).	% ETS table name which holds the http processes' pids
-define(TABLE_PIDS_WS, pids_ws).		% ETS table name which holds the ws processes' pids


% records
-record(state, {
	% tcp
	socket_mode,
	port,
	options,
	acceptors_poolsize,
	recv_timeout,
	max_connections = 1024,			% maximum allowed simultaneous connections
	open_connections_count = 0,
	% misultin
	custom_opts
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok, Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Options) when is_list(Options) ->
	gen_server:start_link(?MODULE, [Options], []).

% Function -> {ok, HttpMonRef} | {error, Reason}
% Description: Accepts the connection if the connection count is not over quota, and add monitor
http_pid_ref_add(ServerRef, HttpPid) ->
	gen_server:call(ServerRef, {http_pid_ref_add, HttpPid}).

% Function -> ok
% Description: Remove a http pid reference from status
http_pid_ref_remove(ServerRef, HttpPid, HttpMonRef) ->
	gen_server:cast(ServerRef, {remove_http_pid, {HttpPid, HttpMonRef}}).

% Function -> ok
% Description: Adds a new websocket pid reference to status
ws_pid_ref_add(ServerRef, WsPid) ->
	gen_server:cast(ServerRef, {add_ws_pid, WsPid}).

% Function -> ok
% Description: Remove a websocket pid reference from status
ws_pid_ref_remove(ServerRef, WsPid) ->
	gen_server:cast(ServerRef, {remove_ws_pid, WsPid}).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([[Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, MaxConnections, SocketMode, CustomOpts]]) ->
	process_flag(trap_exit, true),
	?LOG_INFO("starting misultin server with pid: ~p", [self()]),
	% create ets tables to save open connections and websockets
	ets:new(?TABLE_PIDS_HTTP, [named_table, set, public]),
	ets:new(?TABLE_PIDS_WS, [named_table, set, public]),
	% create listening socket and acceptor
	{ok, #state{socket_mode = SocketMode, port = Port, options = OptionsTcp, acceptors_poolsize = AcceptorsPoolsize, recv_timeout = RecvTimeout, max_connections = MaxConnections, custom_opts = CustomOpts}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% add an http pid reference
handle_call({http_pid_ref_add, HttpPid}, _From, #state{max_connections = MaxConnections, open_connections_count = OpenConnectionsCount} = State) ->
	?LOG_DEBUG("new accept connection request received by http process with pid ~p", [HttpPid]),
	case OpenConnectionsCount >= MaxConnections of
		true ->
			{error, too_many_open_connections};
		false ->
			% add monitor
			HttpMonRef = erlang:monitor(process, HttpPid),
			% add reference in ets table
			ets:insert(?TABLE_PIDS_HTTP, {HttpPid, none}),
			% reply and update status
			{reply, {ok, HttpMonRef}, State#state{open_connections_count = OpenConnectionsCount + 1}}
	end;

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% remove http pid reference from server
handle_cast({remove_http_pid, {HttpPid, HttpMonRef}}, #state{open_connections_count = OpenConnectionsCount} = State) ->
	?LOG_DEBUG("removing http pid reference ~p", [HttpPid]),
	% remove monitor
	catch erlang:demonitor(HttpMonRef),
	% remove pid from ets table
	ets:delete(?TABLE_PIDS_HTTP, HttpPid),
	% return
	{noreply, State#state{open_connections_count = OpenConnectionsCount - 1}};

% add websocket pid reference to server
handle_cast({add_ws_pid, WsPid}, State) ->
	?LOG_DEBUG("adding ws pid reference ~p", [WsPid]),
	ets:insert(?TABLE_PIDS_WS, {WsPid, none}),
	{noreply, State};

% remove websocket pid reference from server
handle_cast({remove_ws_pid, WsPid}, State) ->
	?LOG_DEBUG("removing ws pid reference ~p", [WsPid]),
	ets:delete(?TABLE_PIDS_WS, WsPid),
	{noreply, State};
	
% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?LOG_WARNING("received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% Http process trapping
handle_info({'DOWN', _Ref, process, _HttpPid, normal}, State) -> {noreply, State};	% normal exiting of an http process
handle_info({'DOWN', _Ref, process, HttpPid, _Reason}, #state{open_connections_count = OpenConnectionsCount} = State) ->
	case ets:member(?TABLE_PIDS_HTTP, HttpPid) of
		true ->
			?LOG_ERROR("http process ~p has died with reason: ~p, removing from references of open connections and websockets", [HttpPid, _Reason]),
			ets:delete(?TABLE_PIDS_HTTP, HttpPid),
			ets:delete(?TABLE_PIDS_WS, HttpPid),
			{noreply, State#state{open_connections_count = OpenConnectionsCount - 1}};
		false ->
			?LOG_WARNING("received info on a process ~p crash which is not an http process, with reason: ~p", [HttpPid, _Reason]),
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
terminate(_Reason, _State) ->
	?LOG_INFO("shutting down server with Pid ~p with reason: ~p", [self(), _Reason]),
	% building lists from ets tables
	HttpPidList = [X || {X, _} <- ets:tab2list(?TABLE_PIDS_HTTP)],
	WsPidList = [X || {X, _} <- ets:tab2list(?TABLE_PIDS_WS)],
	% delete ETS tables
	?LOG_INFO("removing ets tables",[]),
	ets:delete(?TABLE_PIDS_HTTP),
	ets:delete(?TABLE_PIDS_WS),
	% send a shutdown message to all websockets, if any
	?LOG_DEBUG("sending shutdown message to ~p websockets", [length(WsPidList)]),
	lists:foreach(fun(WsPid) -> catch WsPid ! shutdown end, WsPidList),
	% force exit of all http processes, if not websockets
	HttpPidRefNoWs = lists:subtract(HttpPidList, WsPidList),
	?LOG_DEBUG("forcing exit of ~p http processes", [length(HttpPidRefNoWs)]),
	lists:foreach(fun(HttpPid) -> exit(HttpPid, kill) end, HttpPidRefNoWs),
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
