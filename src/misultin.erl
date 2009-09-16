% ==========================================================================================================
% MISULTIN - Main
%
% >-|-|-(°>
% 
% Copyright (C) 2009, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
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
-vsn('0.2.2').

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1, stop/0, create_acceptor/0]).

% macros
-define(SERVER, ?MODULE).

% records
-record(state, {
	listen_socket,
	port,
	acceptor,
	loop
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Options) when is_list(Options) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

% Function: -> ok
% Description: Manually stops the server.
stop() ->
	gen_server:cast(?SERVER, stop).

% Function: -> ok
% Description: Send message to cause a new acceptor to be created
create_acceptor() ->
	gen_server:cast(?SERVER, create_acceptor).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([Options]) ->
	process_flag(trap_exit, true),
	?DEBUG(info, "starting with Pid: ~p", [self()]),
	% test and get options
	OptionProps = [
		{ip, "0.0.0.0", fun check_and_convert_string_to_ip/1, invalid_ip},
		{port, 80, fun is_integer/1, port_not_integer},
		{loop, {error, undefined_loop}, fun is_function/1, loop_not_function},
		{backlog, 30, fun is_integer/1, backlog_not_integer}
	],
	OptionsVerified = lists:foldl(fun(OptionName, Acc) -> [get_option(OptionName, Options)|Acc] end, [], OptionProps),
	case proplists:get_value(error, OptionsVerified) of
		undefined ->
			% get options
			Ip = proplists:get_value(ip, OptionsVerified),
			Port = proplists:get_value(port, OptionsVerified),
			Loop = proplists:get_value(loop, OptionsVerified),
			Backlog = proplists:get_value(backlog, OptionsVerified),
			% ipv6 support
			InetOpt = case Ip of
		        {_, _, _, _} ->
					% IPv4
					inet;
		        {_, _, _, _, _, _, _, _} ->
					% IPv6
		            inet6
		    end,
			% ok, no error found in options -> create listening socket.
			% {backlog, 30} specifies the length of the OS accept queue
			% {packet, http} puts the socket into http mode. This makes the socket wait for a HTTP Request line,
			% and if this is received to immediately switch to receiving HTTP header lines. The socket stays in header
			% mode until the end of header marker is received (CR,NL,CR,NL), at which time it goes back to wait for a
			% following HTTP Request line.
			case gen_tcp:listen(Port, [binary, {packet, http}, InetOpt, {ip, Ip}, {reuseaddr, true}, {active, false}, {backlog, Backlog}]) of
				{ok, ListenSocket} ->
					% create first acceptor process
					?DEBUG(debug, "creating first acceptor process", []),
					AcceptorPid = misultin_socket:start_link(ListenSocket, Port, Loop),
					{ok, #state{listen_socket = ListenSocket, port = Port, loop = Loop, acceptor = AcceptorPid}};
				{error, Reason} ->
					?DEBUG(error, "error starting: ~p", [Reason]),
					% error
					{stop, Reason}
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

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% manual shutdown
handle_cast(stop, State) ->
	?DEBUG(info, "manual shutdown..", []),
	{stop, normal, State};

% create
handle_cast(create_acceptor, #state{listen_socket = ListenSocket, port = Port, loop = Loop} = State) ->
	?DEBUG(debug, "creating new acceptor process", []),
	AcceptorPid = misultin_socket:start_link(ListenSocket, Port, Loop),
	{noreply, State#state{acceptor = AcceptorPid}};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?DEBUG(warning, "received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% The current acceptor has died normally, ignore
handle_info({'EXIT', Pid, normal}, #state{acceptor = Pid} = State) ->
	?DEBUG(debug, "current acceptor has died normally", []),
	{noreply, State};

% The current acceptor has died abnormally, wait a little and try again
handle_info({'EXIT', Pid, _Abnormal}, #state{listen_socket = ListenSocket, port = Port, loop = Loop, acceptor = Pid} = State) ->
	?DEBUG(warning, "current acceptor has died with reason: ~p, respawning", [_Abnormal]),
	AcceptorPid = misultin_socket:start_link(ListenSocket, Port, Loop),
	{noreply, State#state{acceptor = AcceptorPid}};

% An acceptor has died, ignore
handle_info({'EXIT', _Pid, _Reason}, State) ->
	?DEBUG(debug, "the acceptor has died with reason: ~p", [_Reason]),
	{noreply, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	?DEBUG(warning, "received unknown info message: ~p", [_Info]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, #state{listen_socket = ListenSocket, acceptor = AcceptorPid}) ->
	?DEBUG(info, "shutting down server with Pid ~p", [self()]),
	% kill acceptor - TODO: find a more gentle way to do so
	exit(AcceptorPid, kill),
	% stop gen_tcp
	gen_tcp:close(ListenSocket),
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
% Description: Checks and converts a string Ip to inet repr.
check_and_convert_string_to_ip(Ip) ->
	case inet_parse:address(Ip) of
		{error, _Reason} ->
			false;
		{ok, IpTuple} ->
			IpTuple
	end.

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

% ============================ /\ INTERNAL FUNCTIONS =======================================================
