% ==========================================================================================================
% MISULTIN - Main Supervisor
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>.
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
-behaviour(supervisor).
-vsn("0.9").

% API
-export([start_link/1, stop/0, stop/1]).

% supervisor callbacks
-export([init/1]).

% macros
-define(SERVER, ?MODULE).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Starts the server.
-spec start_link(Options::gen_proplist()) -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Options) when is_list(Options) ->
	?LOG_DEBUG("starting misultin server",[]),
	% check if name option has been specified, otherwise default to 'misultin' as regname
	case get_option({name, ?SERVER, fun is_atom/1, invalid_misultin_process_name}, Options) of
		{error, Reason} ->
			% option error
			?LOG_ERROR("error in name option: ~p", [Reason]),
			{error, Reason};
		{name, false} ->
			% start misultin without name
			?LOG_DEBUG("starting misultin without a registered name",[]),
			supervisor:start_link(?MODULE, [Options]);
		{name, Value} ->
			% start misultin with specified name
			?LOG_DEBUG("starting misultin with registered name ~p", [Value]),
			Options0 = lists:keyreplace(name, 1, Options, {name, Value}),
			supervisor:start_link({local, Value}, ?MODULE, [Options0])
	end.

% Manually stops the server.
-spec stop() -> true.
-spec stop(SupRef::atom() | pid()) -> true.
stop() ->
	stop(?SERVER).
stop(undefined) ->
	true;
stop(SupName) when is_atom(SupName) ->
	stop(whereis(SupName));
stop(SupPid) when is_pid(SupPid) ->
	?LOG_INFO("shutting down misultin server",[]),
	exit(SupPid, normal).

% ============================ /\ API ======================================================================


% ============================ \/ SUPERVISOR CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok,  {SupFlags,  [ChildSpec]}} | ignore | {error, Reason}
% Description: Starts the supervisor
% ----------------------------------------------------------------------------------------------------------å
-spec init(Options::gen_proplist()) -> {ok, term()} | {error, Reason::term()}.
init([Options]) ->
	?LOG_DEBUG("starting server with pid: ~p", [self()]),
	% test and get options
	OptionProps = [
		% socket
		{ip, {0, 0, 0, 0}, fun check_and_convert_string_to_ip/1, invalid_ip},
		{port, 80, fun is_non_neg_integer/1, port_not_integer},
		{backlog, 128, fun is_non_neg_integer/1, backlog_not_integer},
		{acceptors_poolsize, 10, fun is_non_neg_integer/1, invalid_acceptors_poolsize_option},
		{recv_timeout, 30*1000, fun is_non_neg_integer/1, recv_timeout_not_integer},
		{max_connections, 4096, fun is_non_neg_integer/1, invalid_max_connections_option},
		{ssl, false, fun check_ssl_options/1, invalid_ssl_options},
		{ws_force_ssl, false, fun is_boolean/1, invalid_ssl_external},
		{proxy_protocol, false, fun is_boolean/1, invalid_proxy_protocol},
		{recbuf, default, fun check_recbuf/1, recbuf_not_integer},
		% misultin
		{post_max_size, 4*1024*1024, fun is_non_neg_integer/1, invalid_post_max_size_option},		% defaults to 4 MB
		{get_url_max_size, 2000, fun is_non_neg_integer/1, invalid_get_url_max_size_option},
		{compress, false, fun is_boolean/1, invalid_compress_option},
		{loop, {error, undefined_loop}, fun is_function/1, loop_not_function},
		{autoexit, true, fun is_boolean/1, invalid_autoexit_option},
		{ws_loop, undefined, fun is_function/1, ws_loop_not_function},
		{ws_autoexit, true, fun is_boolean/1, invalid_ws_autoexit_option},
		{ws_versions, ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76'], fun check_ws_version/1, unsupported_ws_vsn_specified},
		{sessions_expire, 600, fun is_non_neg_integer/1, invalid_sessions_expire_option},
		{access_log, undefined, fun check_access_log/1, invalid_access_log_option},
		{auto_recv_body, true, fun is_boolean/1, invalid_auto_recv_body_option},
		{static, false, fun check_static/1, invalid_static_option}		% if set to a directory, requests to /static/* will automatically send files from the directory
	],
	OptionsVerified = lists:foldl(fun(OptionProp, Acc) -> [get_option(OptionProp, Options)|Acc] end, [], OptionProps),
	case proplists:get_value(error, OptionsVerified) of
		undefined ->
			% ok, no error found in options
			% tcp options
			Ip = proplists:get_value(ip, OptionsVerified),
			Port = proplists:get_value(port, OptionsVerified),
			Backlog = proplists:get_value(backlog, OptionsVerified),
			AcceptorsPoolsize = proplists:get_value(acceptors_poolsize, OptionsVerified),
			RecvTimeout = proplists:get_value(recv_timeout, OptionsVerified),
			MaxConnections = proplists:get_value(max_connections, OptionsVerified),
			SslOptions0 = proplists:get_value(ssl, OptionsVerified),
			WsForceSsl = proplists:get_value(ws_force_ssl, OptionsVerified),
			ProxyProtocol = proplists:get_value(proxy_protocol, OptionsVerified),
			% misultin options
			PostMaxSize = proplists:get_value(post_max_size, OptionsVerified),
			GetUrlMaxSize = proplists:get_value(get_url_max_size, OptionsVerified),
			Compress = proplists:get_value(compress, OptionsVerified),
			Loop = proplists:get_value(loop, OptionsVerified),
			AutoExit = proplists:get_value(autoexit, OptionsVerified),
			WsLoop = proplists:get_value(ws_loop, OptionsVerified),
			WsAutoExit = proplists:get_value(ws_autoexit, OptionsVerified),
			WsVersions = proplists:get_value(ws_versions, OptionsVerified),
			RecBuf = proplists:get_value(recbuf, OptionsVerified),
			SessionsExpireSec = proplists:get_value(sessions_expire, OptionsVerified),
			AccessLogFun = proplists:get_value(access_log, OptionsVerified),
			AutoRecvBody = proplists:get_value(auto_recv_body, OptionsVerified),
			Static = proplists:get_value(static, OptionsVerified),
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
					OptionsTcp0 = [binary, {packet, raw}, {ip, Ip}, {reuseaddr, true}, {active, false}, {backlog, Backlog}|AdditionalOptions],
					OptionsTcp = case RecBuf of
						default -> OptionsTcp0;
						_ -> [{recbuf, RecBuf}|OptionsTcp0]
					end,							
					% build custom_opts
					CustomOpts = #custom_opts{
						post_max_size = PostMaxSize,
						get_url_max_size = GetUrlMaxSize,
						compress = Compress,
						loop = Loop,
						autoexit = AutoExit,
						ws_loop = WsLoop,
						ws_autoexit = WsAutoExit,
						ws_versions = WsVersions,
						access_log = AccessLogFun,
						ws_force_ssl = WsForceSsl,
						proxy_protocol = ProxyProtocol,
						auto_recv_body = AutoRecvBody,
						static = Static
					},
					% define misultin_server supervisor specs
					ServerSpec = {server, {misultin_server, start_link, [{MaxConnections}]}, permanent, 60000, worker, [misultin_server]},
					% define sessions supervisor
					SessionsSpec = {sessions, {misultin_sessions, start_link, [{self(), SessionsExpireSec}]}, permanent, 60000, worker, [misultin_sessions]},
					% define acceptors supervisor specs
					AcceptorSupSpec = {acceptors_sup, {misultin_acceptors_sup, start_link, [self(), Port, OptionsTcp, AcceptorsPoolsize, RecvTimeout, SocketMode, CustomOpts]}, permanent, infinity, supervisor, [misultin_acceptors_sup]},
					% ip address
					?LOG_INFO("starting misultin server on address ~s and port ~p", [misultin_utility:convert_ip_to_list(Ip), Port]),
					% spawn
					{ok, {{one_for_all, 5, 30}, [ServerSpec, SessionsSpec, AcceptorSupSpec]}};
				Error ->
					?LOG_ERROR("error starting misultin server: ~p", [Error]),
					{error, Error}
			end;
		Reason ->
			% error found in options
			?LOG_ERROR("option error starting misultin server: ~p", [Reason]),
			{error, Reason}
	end.

% ============================ /\ SUPERVISOR CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Checks and if necessary converts a string Ip to inet repr.
-spec check_and_convert_string_to_ip(Ip::string() | tuple()) -> inet:ip_address() | false.
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
	
% Checks if all necessary Ssl Options have been specified
-spec check_ssl_options(SslOptions::gen_proplist()) -> boolean().
check_ssl_options(SslOptions) ->
	Opts = [verify, fail_if_no_peer_cert, verify_fun, depth, certfile, keyfile, password, cacertfile, ciphers, reuse_sessions, reuse_session],
	F = fun({Name, _Value}) ->
		case lists:member(Name, Opts) of
			false -> false;
			_ -> true
		end
	end,
	lists:all(F, SslOptions).

% check if recbuf has been set
-spec check_recbuf(RecBuf::default | non_neg_integer()) -> boolean().
check_recbuf(default) -> default;
check_recbuf(RecBuf) when is_integer(RecBuf), RecBuf > 0 -> true;
check_recbuf(_RecBuf) -> false.

% check if access log fun has been set
-spec check_access_log(AccessLogFun::false | function()) -> boolean().
check_access_log(undefined) -> true;
check_access_log(AccessLogFun) when is_function(AccessLogFun) -> true;
check_access_log(_AccessLogFun) -> false.

% check if ws specified versions are implemented. order does matter so we build a list in proper order
-spec check_ws_version([websocket_version()]) -> false | [websocket_version()].
check_ws_version(WsVsn) ->
	ImplementedVsn = ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'],
	%  build an ordered list of supported versions chosen by user.
	F = fun(SupportedVsn, Acc) ->
		case lists:member(SupportedVsn, WsVsn) of
			true -> [SupportedVsn|Acc];
			false -> Acc
		end
	end,
	OrderedVsn = lists:reverse(lists:foldl(F, [], ImplementedVsn)),
	% if length do not agree, then user has specified an unsupported version
	case length(OrderedVsn) =:= length(WsVsn) of
		true -> OrderedVsn;
		_ -> false
	end.

% check if a number is a non negative integer
-spec is_non_neg_integer(term()) -> boolean().
is_non_neg_integer(N) when is_integer(N), N >= 0 -> true;
is_non_neg_integer(_) -> false.

% check if the static option is valid
-spec check_static(list() | false) -> boolean().
check_static(false) -> true;
check_static(Path) when is_list(Path) -> true;
check_static(_) -> false.

% Validate and get misultin options.
-spec get_option({
	OptionName::atom(),
	DefaultValue::term(),
	CheckAndConvertFun::function(),
	FailTypeError::term()
	}, Options::gen_proplist()) -> misultin_option() | {error, Reason::term()}.
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

% Start an application.
-spec start_application(Application::atom()) -> ok | {error, Reason::term()}.
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

% ============================ /\ INTERNAL FUNCTIONS =======================================================
