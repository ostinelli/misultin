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
-vsn("0.9").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1]).
-export([http_pid_ref_add/2, ws_pid_ref_add/2]).
-export([get_rfc_date/1, get_iso8601_date/1, get_timestamp/1, get_table_date_ref/1]).

% macros
-define(TABLE_PIDS_HTTP, misultin_table_pids_http).
-define(TABLE_PIDS_WS, misultin_table_pids_ws).
-define(TABLE_DATE, misultin_table_date).
-define(DATE_UPDATE_INTERVAL, 1000).	   % update interval in ms for RFC time [used in Date headers]

% records
-record(state, {
	max_connections			= undefined :: undefined | non_neg_integer(),	% maximum allowed simultaneous connections
	open_connections_count	= 0 :: non_neg_integer(),						% current number of open connections
	table_pids_http			= undefined :: undefined | ets:tid(),			% ETS table reference which holds the http processes' pids
	table_pids_ws			= undefined :: undefined | ets:tid(),			% ETS table reference which holds the ws processes' pids
	table_date				= undefined :: undefined | ets:tid()			% ETS table reference which holds the RFC date
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the server.
-spec start_link(term()) -> {ok, Pid::pid()} | {error, Error::term()}.
start_link(Options) when is_tuple(Options) ->
	gen_server:start_link(?MODULE, Options, []).

% Accepts the connection if the connection count is not over quota, and add monitor
-spec http_pid_ref_add(ServerRef::pid(), HttpPid::pid()) -> ok | {error, too_many_open_connections}.
http_pid_ref_add(ServerRef, HttpPid) ->
	gen_server:call(ServerRef, {http_pid_ref_add, HttpPid}).

% Adds a new websocket pid reference to status
-spec ws_pid_ref_add(ServerRef::pid(), WsPid::pid()) -> ok.
ws_pid_ref_add(ServerRef, WsPid) ->
	gen_server:cast(ServerRef, {add_ws_pid, WsPid}).

% Retrieve computed RFC date
-spec get_rfc_date(TableDateRef::ets:tid()) -> string().
get_rfc_date(TableDateRef) ->
	ets:lookup_element(TableDateRef, rfc_date, 2).
	
% Retrieve computed ISO 8601 date
-spec get_iso8601_date(TableDateRef::ets:tid()) -> string().
get_iso8601_date(TableDateRef) ->
	ets:lookup_element(TableDateRef, iso_8601_date, 2).
	
% Retrieve computed timestamp
-spec get_timestamp(TableDateRef::ets:tid()) -> non_neg_integer().
get_timestamp(TableDateRef) ->
	ets:lookup_element(TableDateRef, timestamp, 2).	

% Retrieve table date reference.
-spec get_table_date_ref(ServerRef::pid()) -> TableDateRef::ets:tid().
get_table_date_ref(ServerRef) ->
	gen_server:call(ServerRef, get_table_date_ref).
	
% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init({MaxConnections}) ->
	process_flag(trap_exit, true),
	?LOG_DEBUG("starting misultin server with pid: ~p", [self()]),
	% create ets tables to save open connections and websockets
	TablePidsHttp = ets:new(?TABLE_PIDS_HTTP, [set, protected]),
	TablePidsWs = ets:new(?TABLE_PIDS_WS, [set, protected]),
	% create ets table to hold RFC date information, and fill it with startup info
	TableDate = try
		ets:new(?TABLE_DATE, [set, public, {read_concurrency, true}])
	catch
		error:badarg ->
			% read_concurrency not supported before R14B
			ets:new(?TABLE_DATE, [set, public])
	end,
	update_datetable(TableDate),
	% start date build timer
	erlang:send_after(?DATE_UPDATE_INTERVAL, self(), compute_rfc_date),
	% return
	{ok, #state{
		max_connections = MaxConnections,
		table_pids_http = TablePidsHttp,
		table_pids_ws = TablePidsWs,
		table_date = TableDate
	}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% add an http pid reference
handle_call({http_pid_ref_add, HttpPid}, _From, #state{max_connections = MaxConnections, open_connections_count = OpenConnectionsCount, table_pids_http = TablePidsHttp} = State) ->
	?LOG_DEBUG("new accept connection request received by http process with pid ~p", [HttpPid]),
	case OpenConnectionsCount >= MaxConnections of
		true ->
			?LOG_WARNING("too many open connections, refusing new one",[]),
			% reply
			{reply, {error, too_many_open_connections}, State};
		false ->
			% add reference in ets table
			ets:insert(TablePidsHttp, {HttpPid}),
			% add monitor
			erlang:monitor(process, HttpPid),
			% update counter
			NewOpenConnectionCount = OpenConnectionsCount + 1,
			?LOG_DEBUG("open connection count is ~p, http processes are: ~p, ws processes are: ~p", [NewOpenConnectionCount, ets:tab2list(TablePidsHttp), ets:tab2list(State#state.table_pids_ws)]),
			% reply
			{reply, ok, State#state{open_connections_count = NewOpenConnectionCount}}
	end;

% get table date reference
handle_call(get_table_date_ref, _From, #state{table_date = TableDate} = State) ->
	{reply, TableDate, State};

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	?LOG_WARNING("received unknown call message: ~p", [_Request]),
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% add websocket pid reference to server
handle_cast({add_ws_pid, WsPid}, #state{table_pids_ws = TablePidsWs, table_pids_http = TablePidsHttp} = State) ->
	?LOG_DEBUG("adding ws pid reference ~p", [WsPid]),
	% add to ws processes list
	ets:insert(TablePidsWs, {WsPid}),
	% remove from http processes list
	ets:delete(TablePidsHttp, WsPid),
	?LOG_DEBUG("open connection count is ~p, http processes are: ~p, ws processes are: ~p", [State#state.open_connections_count, ets:tab2list(TablePidsHttp), ets:tab2list(TablePidsWs)]),		
	{noreply, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?LOG_WARNING("received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% compute RFC date
handle_info(compute_rfc_date, #state{table_date = TableDate} = State) ->
	% avoid locking gen server
	Self = self(),
	spawn(fun() ->
		% update date
		update_datetable(TableDate),
		% start date build timer
		erlang:send_after(?DATE_UPDATE_INTERVAL, Self, compute_rfc_date)
	end),
	{noreply, State};

% Http close/crash process trapping
handle_info({'DOWN', _Ref, process, HttpOrWsPid, Reason}, #state{table_pids_http = TablePidsHttp, table_pids_ws = TablePidsWs, open_connections_count = OpenConnectionsCount} = State) ->
	% check if pid is http process
	case ets:member(TablePidsHttp, HttpOrWsPid) of
		true ->
			% log
			case Reason of
				normal -> ?LOG_DEBUG("http process ~p died normally, removing from references of open connections", [HttpOrWsPid]);
				_ -> ?LOG_ERROR("http process ~p has died with reason: ~p, removing from references of open connections", [HttpOrWsPid, Reason])
			end,
			% delete from ets
			ets:delete(TablePidsHttp, HttpOrWsPid),
			ets:delete(TablePidsWs, HttpOrWsPid),	% this is just being a little paranoid, should never happen
			% decrease counter
			NewOpenConnectionCount = OpenConnectionsCount - 1,
			?LOG_DEBUG("open connection count is ~p, http processes are: ~p, ws processes are: ~p", [NewOpenConnectionCount, ets:tab2list(TablePidsHttp), ets:tab2list(TablePidsWs)]),		
			{noreply, State#state{open_connections_count = NewOpenConnectionCount}};
		false ->
			% check if is ws process
			case ets:member(TablePidsWs, HttpOrWsPid) of
				true ->
					% log
					case Reason of
						normal -> ?LOG_DEBUG("ws process ~p died normally, removing from references of open websockets", [HttpOrWsPid]);
						_ -> ?LOG_ERROR("ws process ~p has died with reason: ~p, removing from references of open websockets", [HttpOrWsPid, Reason])
					end,
					% delete from ets
					ets:delete(TablePidsWs, HttpOrWsPid),
					% decrease counter
					NewOpenConnectionCount = OpenConnectionsCount - 1,
					?LOG_DEBUG("open connection count is ~p, http processes are: ~p, ws processes are: ~p", [NewOpenConnectionCount, ets:tab2list(TablePidsHttp), ets:tab2list(TablePidsWs)]),		
					{noreply, State#state{open_connections_count = NewOpenConnectionCount}};
				false ->
					?LOG_WARNING("received info on a process ~p crash which is not an http process, with reason: ~p", [HttpOrWsPid, Reason]),
					{noreply, State}
			end
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
terminate(_Reason, #state{table_pids_http = TablePidsHttp, table_pids_ws = TablePidsWs, table_date = TableDate}) ->
	?LOG_DEBUG("shutting down server with Pid ~p with reason: ~p", [self(), _Reason]),
	% build pid lists from ets tables
	HttpPidList = [X || {X} <- ets:tab2list(TablePidsHttp)],
	WsPidList = [X || {X} <- ets:tab2list(TablePidsWs)],
	% send a shutdown message to all websockets, if any
	?LOG_DEBUG("sending shutdown message to ~p websockets", [length(WsPidList)]),
	lists:foreach(fun(WsPid) -> catch WsPid ! shutdown end, WsPidList),
	% fsend a shutdown message to all http processes
	?LOG_DEBUG("sending shutdown message to ~p http processes", [length(HttpPidList)]),
	lists:foreach(fun(HttpPid) -> catch HttpPid ! shutdown end, HttpPidList),
	% delete ETS tables
	?LOG_DEBUG("removing ets tables",[]),
	ets:delete(TablePidsHttp),
	ets:delete(TablePidsWs),
	ets:delete(TableDate),
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% compute RFC and ISO8601 dates and update ETS table
-spec update_datetable(TableDate::ets:tid()) -> true.
update_datetable(TableDate) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:universal_time(),
	ets:insert(TableDate, [
		{rfc_date, httpd_util:rfc1123_date({{Year,Month,Day},{Hour,Min,Sec}})},
		{iso_8601_date, lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B UTC", [Year, Month, Day, Hour, Min, Sec]))},
		{timestamp, misultin_utility:get_unix_timestamp()}
	]).

% ============================ /\ INTERNAL FUNCTIONS =======================================================
