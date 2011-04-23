% ==========================================================================================================
% MISULTIN - Main
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
-module(misultin).
-behaviour(gen_server).
-vsn("0.7.1-dev").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1, stop/0, stop/1]).
-export([get_open_connections_count/1, http_pid_ref_add/2, http_pid_ref_remove/3, ws_pid_ref_add/2, ws_pid_ref_remove/2]).

% macros
-define(SERVER, ?MODULE).
-define(APPLICATION_START_RETRYAFTER, 1000).	% ms to check that a required application has been started
-define(APPLICATION_START_RETRYCOUNT, 10).		% retry count to check that a required application has been started

% records
-record(state, {
	% tcp
	listen_socket,
	socket_mode,
	port,
	options,
	acceptor,
	recv_timeout,
	max_connections = 1024,			% maximum allowed simultaneous connections
	open_connections_count = 0,
	http_pid_ref = [],
	ws_pid_ref = [],
	% misultin
	custom_opts
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok, Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Options) when is_list(Options) ->
	% check if name option has been specified, otherwise default to 'misultin' as regname
	case get_option({name, ?SERVER, fun is_atom/1, invalid_misultin_process_name}, Options) of
		{error, Reason} ->
			% option error
			?LOG_ERROR("error in name option: ~p", [Reason]),
			{error, Reason};
		{name, false} ->
			% start misultin without name
			?LOG_DEBUG("starting misultin without a registered name",[]),
			gen_server:start_link(?MODULE, [Options], []);
		{name, Value} ->
			% start misultin with specified name
			?LOG_DEBUG("starting misultin with registered name ~p", [Value]),
			gen_server:start_link({local, Value}, ?MODULE, [Options], [])
	end.

% Function: -> ok
% Description: Manually stops the server.
stop() ->
	stop(?SERVER).
stop(ServerRef) ->
	gen_server:cast(ServerRef, stop).

% Function -> integer()
% Description: Gets the count of the current open connections
get_open_connections_count(ServerRef) ->
	gen_server:call(ServerRef, get_open_connections_count).

% Function -> ok
% Description: Adds a new http pid reference to status
http_pid_ref_add(ServerRef, HttpPid) ->
	gen_server:cast(ServerRef, {add_http_pid, HttpPid}).

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
init([Options]) ->
	process_flag(trap_exit, true),
	?LOG_INFO("starting with Pid: ~p", [self()]),
	% test and get options
	OptionProps = [
		% socket
		{ip, {0, 0, 0, 0}, fun check_and_convert_string_to_ip/1, invalid_ip},
		{port, 80, fun is_integer/1, port_not_integer},
		{backlog, 128, fun is_integer/1, backlog_not_integer},
		{recv_timeout, 30*1000, fun is_integer/1, recv_timeout_not_integer},
		{max_connections, 1024, fun is_integer/1, invalid_max_connections_option},
		{ssl, false, fun check_ssl_options/1, invalid_ssl_options},
		% misultin
		{post_max_size, 4*1012*1012, fun is_integer/1, invalid_post_max_size_option},		% defaults to 4 MB
		{get_url_max_size, 2000, fun is_integer/1, invalid_get_url_max_size_option},
		{compress, false, fun is_boolean/1, invalid_compress_option},
		{loop, {error, undefined_loop}, fun is_function/1, loop_not_function},
		{autoexit, true, fun is_boolean/1, invalid_autoexit_option},
		{ws_loop, none, fun is_function/1, ws_loop_not_function},
		{ws_autoexit, true, fun is_boolean/1, invalid_ws_autoexit_option}
	],
	OptionsVerified = lists:foldl(fun(OptionProp, Acc) -> [get_option(OptionProp, Options)|Acc] end, [], OptionProps),
	case proplists:get_value(error, OptionsVerified) of
		undefined ->
			% ok, no error found in options
			% tcp options
			Ip = proplists:get_value(ip, OptionsVerified),
			Port = proplists:get_value(port, OptionsVerified),
			Backlog = proplists:get_value(backlog, OptionsVerified),
			RecvTimeout = proplists:get_value(recv_timeout, OptionsVerified),
			MaxConnections = proplists:get_value(max_connections, OptionsVerified),
			SslOptions0 = proplists:get_value(ssl, OptionsVerified),
			% misultin options
			PostMaxSize = proplists:get_value(post_max_size, OptionsVerified),
			GetUrlMaxSize = proplists:get_value(get_url_max_size, OptionsVerified),
			Compress = proplists:get_value(compress, OptionsVerified),
			Loop = proplists:get_value(loop, OptionsVerified),
			AutoExit = proplists:get_value(autoexit, OptionsVerified),
			WsLoop = proplists:get_value(ws_loop, OptionsVerified),
			WsAutoExit = proplists:get_value(ws_autoexit, OptionsVerified),
			% ipv6 support
			?LOG_DEBUG("ip address is: ~p", [Ip]),
			% set additional options according to socket mode if necessary
			Continue = case SslOptions0 of
				false ->
					% without SSL
					SocketMode = http,
					InetOpt = case Ip of
						{_, _, _, _} ->
							% IPv4
							inet;
						{_, _, _, _, _, _, _, _} ->
							% IPv6
							inet6
					end,
					AdditionalOptions = [InetOpt],
					true;
				_ ->
					% with SSL
					SocketMode = ssl,
					% the only current way to use {active, once} in Ssl is to start the crypto module
					% and set {ssl_imp, new} as SSL option, see
					% <http://www.erlang.org/cgi-bin/ezmlm-cgi?4:mss:50633:201004:fpopocbfkpppecdembbe>
					AdditionalOptions = [{ssl_imp, new}|SslOptions0],
					% start Ssl and crypto applications if necessary, and get outcomes
					AppStartResults = lists:keyfind(error, 1, [start_application(crypto), start_application(public_key), start_application(ssl)]),
					case AppStartResults of
						false ->
							% all applications started succesfully
							true;
						_ ->
							% error starting application
							{error, AppStartResults}
					end
			end,
			% proceed?
			case Continue of
				true ->
					% set options
					OptionsTcp = [binary, {packet, raw}, {ip, Ip}, {reuseaddr, true}, {active, false}, {backlog, Backlog}|AdditionalOptions],
					% build custom_opts
					CustomOpts = #custom_opts{post_max_size = PostMaxSize, get_url_max_size = GetUrlMaxSize, compress = Compress, loop = Loop, autoexit = AutoExit, ws_loop = WsLoop, ws_autoexit = WsAutoExit},
					% create listening socket and acceptor
					case create_listener_and_acceptor(Port, OptionsTcp, RecvTimeout, MaxConnections, SocketMode, CustomOpts) of
						{ok, ListenSocket, AcceptorPid} ->
							?LOG_DEBUG("listening socket and acceptor succesfully started",[]),
							{ok, #state{listen_socket = ListenSocket, socket_mode = SocketMode, port = Port, options = OptionsTcp, acceptor = AcceptorPid, recv_timeout = RecvTimeout, max_connections = MaxConnections, custom_opts = CustomOpts}};
						{error, Reason} ->
							?LOG_ERROR("error starting listener socket: ~p", [Reason]),
							{stop, Reason}
					end;
				Error ->
					{stop, Error}
			end;
		Reason ->
			% error found in options
			{stop, Reason}
	end.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% current open connections count
handle_call(get_open_connections_count, _From, #state{open_connections_count = OpenConnectionsCount} = State) ->
	{reply, OpenConnectionsCount, State};

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% manual shutdown
handle_cast(stop, State) ->
	?LOG_INFO("manual shutdown..", []),
	{stop, normal, State};

% add a new http pid reference to status
handle_cast({add_http_pid, HttpPid}, #state{open_connections_count = OpenConnectionsCount, http_pid_ref = HttpPidRef} = State) ->
	?LOG_DEBUG("adding http pid reference ~p", [HttpPid]),
	% add monitor
	erlang:monitor(process, HttpPid),
	% return
	{noreply, State#state{open_connections_count = OpenConnectionsCount + 1, http_pid_ref = [HttpPid|HttpPidRef]}};

% remove http pid reference from server
handle_cast({remove_http_pid, {HttpPid, HttpMonRef}}, #state{open_connections_count = OpenConnectionsCount, http_pid_ref = HttpPidRef} = State) ->
	?LOG_DEBUG("removing http pid reference ~p", [HttpPid]),
	% remove monitor
	catch erlang:demonitor(HttpMonRef),	
	% return
	{noreply, State#state{open_connections_count = OpenConnectionsCount - 1, http_pid_ref = lists:delete(HttpPid, HttpPidRef)}};

% add websocket pid reference to server
handle_cast({add_ws_pid, WsPid}, #state{ws_pid_ref = WsPidRef} = State) ->
	?LOG_DEBUG("adding ws pid reference ~p", [WsPid]),
	{noreply, State#state{ws_pid_ref = [WsPid|WsPidRef]}};

% remove websocket pid reference from server
handle_cast({remove_ws_pid, WsPid}, #state{ws_pid_ref = WsPidRef} = State) ->
	?LOG_DEBUG("removing ws pid reference ~p", [WsPid]),
	{noreply, State#state{ws_pid_ref = lists:delete(WsPid, WsPidRef)}};
	
% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?LOG_WARNING("received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% Acceptor died
% -> shutdown in progress, ignore
handle_info({'EXIT', Pid, {error, {{accept_failed, {shutdown, _}}}}}, #state{acceptor = Pid} = State) -> {noreply, State};
% -> respawn listening socket and acceptor
handle_info({'EXIT', Pid, _Reason}, #state{listen_socket = ListenSocket, socket_mode = SocketMode, port = Port, acceptor = Pid, recv_timeout = RecvTimeout, max_connections = MaxConnections, custom_opts = CustomOpts} = State) ->
	?LOG_ERROR("acceptor has died with reason: ~p, respawning", [_Reason]),
	AcceptorPid = misultin_socket:acceptor_start_link(self(), ListenSocket, Port, RecvTimeout, MaxConnections, SocketMode, CustomOpts),
	{noreply, State#state{acceptor = AcceptorPid}};

% Http process trapping
handle_info({'DOWN', _Ref, process, _HttpPid, normal}, State) -> {noreply, State};	% normal exiting of an http process
handle_info({'DOWN', _Ref, process, HttpPid, _Reason}, #state{open_connections_count = OpenConnectionsCount, http_pid_ref = HttpPidRef, ws_pid_ref = WsPidRef} = State) ->
	State0 = case lists:member(HttpPid, HttpPidRef) of
		true ->
			?LOG_ERROR("http process ~p has died with reason: ~p, removing from references of open connections", [HttpPid, _Reason]),
			State#state{open_connections_count = OpenConnectionsCount - 1, http_pid_ref = lists:delete(HttpPid, HttpPidRef)};
		false ->
			?LOG_WARNING("received info on a process ~p crash which is not an http process, with reason: ~p", [HttpPid, _Reason]),
			State
	end,
	% no checking done to improve performance
	?LOG_DEBUG("open references are: ~p, count is now ~p, trying to remove from websocket open references", [State0#state.http_pid_ref, State0#state.open_connections_count]),
	{noreply, State0#state{ws_pid_ref = lists:delete(HttpPid, WsPidRef)}};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	?LOG_WARNING("received unknown info message: ~p", [_Info]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, #state{listen_socket = ListenSocket, socket_mode = SocketMode, acceptor = AcceptorPid, http_pid_ref = HttpPidRef, ws_pid_ref = WsPidRef}) ->
	?LOG_INFO("shutting down server with Pid ~p with reason: ~p", [self(), _Reason]),
	% kill acceptor
	exit(AcceptorPid, kill),
	% send a shutdown message to all websockets, if any
	?LOG_DEBUG("sending shutdown message to websockets: ~p", [WsPidRef]),
	lists:foreach(fun(WsPid) -> catch WsPid ! shutdown end, WsPidRef),
	% force exit of all http processes, if not websockets
	HttpPidRefNoWs = lists:subtract(HttpPidRef, WsPidRef),
	?LOG_DEBUG("forcing exit of http processes: ~p", [HttpPidRefNoWs]),
	lists:foreach(fun(HttpPid) -> exit(HttpPid, kill) end, HttpPidRefNoWs),
	% stop tcp socket
	misultin_socket:close(ListenSocket, SocketMode),
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Function: -> false | IpTuple
% Description: Checks and if necessary converts a string Ip to inet repr.
check_and_convert_string_to_ip(Ip) when is_tuple(Ip) ->
	case size(Ip) of
		4 ->
			% check for valid ipv4
			LIp = [Num || Num <- tuple_to_list(Ip), Num >= 0, Num =< 255],
			length(LIp) =:= 4 andalso Ip;
		8 ->
			% check for valid ipv6
			LIp = [Num || Num <- tuple_to_list(Ip), Num >= 0, Num =< 16#FFFF],
			length(LIp) =:= 8 andalso Ip;
		_ ->
			false
	end;
check_and_convert_string_to_ip(Ip) ->
	case inet_parse:address(Ip) of
		{error, _Reason} ->
			false;
		{ok, IpTuple} ->
			IpTuple
	end.
	
% Function: -> true | false
% Description: Checks if all necessary Ssl Options have been specified
check_ssl_options(SslOptions) ->
	Opts = [verify, fail_if_no_peer_cert, verify_fun, depth, certfile, keyfile, password, cacertfile, ciphers, reuse_sessions, reuse_session],
	F = fun({Name, _Value}) ->
		case lists:member(Name, Opts) of
			false -> false;
			_ -> true
		end
	end,
	lists:all(F, SslOptions).

% Description: Validate and get misultin options.
get_option({OptionName, DefaultValue, CheckAndConvertFun, FailTypeError}, Options) ->
	case proplists:get_value(OptionName, Options) of
		undefined ->
			case DefaultValue of
				{error, Reason} ->
					{error, Reason};
				Value -> 
					{OptionName, Value}
			end;
		Value ->
			case CheckAndConvertFun(Value) of
				false ->
					{error, {FailTypeError, Value}};
				true -> 
					{OptionName, Value};
				OutValue ->
					{OptionName, OutValue}
			end
	end.

% Function: -> ok | {error, Reason}
% Description: Start an application.
start_application(Application) ->
	case lists:keyfind(Application, 1, application:which_applications()) of
		false ->
			?LOG_DEBUG("starting application ~p", [Application]),
			case application:start(Application) of
				ok ->
					ok;
				{error, Reason} ->
					?LOG_ERROR("error starting application ~p", [Application]),
					{error, Reason}
			end;
		_ ->
			?LOG_DEBUG("application ~p is already running, skip", [Application]),
			ok
	end.

% Function: -> {ok, ListenSocket, AcceptorPid} | {error, Error}
% Description: Create listening socket
create_listener_and_acceptor(Port, Options, RecvTimeout, MaxConnections, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting listening ~p socket with options ~p on port ~p", [SocketMode, Options, Port]),
	case misultin_socket:listen(Port, Options, SocketMode) of
		{ok, ListenSocket} ->
			?LOG_DEBUG("starting acceptor",[]),
			% create acceptor
			AcceptorPid = misultin_socket:acceptor_start_link(self(), ListenSocket, Port, RecvTimeout, MaxConnections, SocketMode, CustomOpts),
			{ok, ListenSocket, AcceptorPid};
		{error, Reason} ->
			% error
			{error, Reason}
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
