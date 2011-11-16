% ==========================================================================================================
% MISULTIN - WebSocket - draft hybi 10
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>,
%                     portions of code from Andy W. Song <https://github.com/awsong/erl_websocket>
% All rights reserved.
%
% Code portions from Joe Armstrong have been originally taken under MIT license at the address:
% <http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html>
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
-module('misultin_websocket_draft-hybi-17').
-behaviour(misultin_websocket).
-vsn("0.9-dev").

% API
-export([check_websocket/1, handshake/3, handle_data/3, send_format/2]).

% records
-record(state, {
	buffer,
	mask_key = <<0,0,0,0>>
}).

% macros
-define(OP_CONT, 0).
-define(OP_TEXT, 1).
-define(OP_BIN, 2).
-define(OP_CLOSE, 8).
-define(OP_PING, 9).
-define(OP_PONG, 10).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> true | false
% Description: Callback to check if the incoming request is a websocket request according to this protocol.
% ----------------------------------------------------------------------------------------------------------
-spec check_websocket(Headers::http_headers()) -> boolean().
check_websocket(Headers) ->
	% set required headers
	RequiredHeaders = [
		{'Upgrade', "websocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, 
		{'Sec-Websocket-Key', ignore}, {'Sec-WebSocket-Version', "13"}
	],
	% check for headers existance
	case misultin_websocket:check_headers(Headers, RequiredHeaders) of
		true -> true;
		_RemainingHeaders ->
			?LOG_DEBUG("not this protocol, remaining headers: ~p", [_RemainingHeaders]),
			false
	end.

% ----------------------------------------------------------------------------------------------------------
% Function: -> iolist() | binary()
% Description: Callback to build handshake data.
% ----------------------------------------------------------------------------------------------------------
-spec handshake(Req::#req{}, Headers::http_headers(), {Path::string(), Origin::string(), Host::string()}) -> iolist().
handshake(_Req, Headers, {_Path, _Origin, _Host}) ->
	% build data
	Key = list_to_binary(misultin_utility:header_get_value('Sec-WebSocket-Key', Headers)),
	Accept = base64:encode_to_string(crypto:sha(<<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
	["HTTP/1.1 101 Switching Protocols\r\n",
		"Upgrade: websocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Accept: ", Accept, "\r\n\r\n"
	].

% ----------------------------------------------------------------------------------------------------------
% Function: -> websocket_close | {websocket_close, DataToSendBeforeClose::binary() | iolist()} | NewStatus
% Description: Callback to handle incomed data.
% ----------------------------------------------------------------------------------------------------------
-spec handle_data(Data::binary(), Status::undefined | term(), {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | term().
handle_data(Data, undefined, {Socket, SocketMode, WsHandleLoopPid}) ->
	% init status
	handle_data(Data, #state{buffer = none}, {Socket, SocketMode, WsHandleLoopPid});
handle_data(Data, State, {Socket, SocketMode, WsHandleLoopPid}) ->
	% read status
	i_handle_data(Data, State, {Socket, SocketMode, WsHandleLoopPid}).

% ----------------------------------------------------------------------------------------------------------
% Function: -> binary() | iolist()
% Description: Callback to format data before it is sent into the socket.
% ----------------------------------------------------------------------------------------------------------
-spec send_format(Data::iolist(), Status::term()) -> binary().
send_format(Data, _State) ->
	send_format(Data, ?OP_TEXT, _State).
send_format(Data, OpCode, _State) ->
	BData = erlang:iolist_to_binary(Data),
	Len = erlang:size(BData),
	if
		Len < 126 ->
			<<1:1, 0:3, OpCode:4, 0:1, Len:7, BData/binary>>;
		Len < 65536 ->
			<<1:1, 0:3, OpCode:4, 0:1, 126:7, Len:16, BData/binary>>;
		true ->
			<<1:1, 0:3, OpCode:4, 0:1, 127:7, 0:1, Len:63, BData/binary>>
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

%      0                   1                   2                   3
%      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%     +-+-+-+-+-------+-+-------------+-------------------------------+
%     |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
%     |I|S|S|S|  (4)  |A|     (7)     |             (16/63)           |
%     |N|V|V|V|       |S|             |   (if payload len==126/127)   |
%     | |1|2|3|       |K|             |                               |
%     +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
%     |     Extended payload length continued, if payload len == 127  |
%     + - - - - - - - - - - - - - - - +-------------------------------+
%     |                               |Masking-key, if MASK set to 1  |
%     +-------------------------------+-------------------------------+
%     | Masking-key (continued)       |          Payload Data         |
%     +-------------------------------- - - - - - - - - - - - - - - - +
%     :                     Payload Data continued ...                :
%     + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%     |                     Payload Data continued ...                |
%     +---------------------------------------------------------------+

% handle incomed data
-spec i_handle_data(Data::binary(), State::undefined | term(), {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | term().
i_handle_data(Data, #state{buffer = Buffer} = State, {Socket, SocketMode, WsHandleLoopPid}) when is_binary(Buffer) ->
    i_handle_data(<<Buffer/binary, Data/binary>>, State#state{buffer = none}, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<Fin:1, 0:3, Opcode:4, 1:1, PayloadLen:7, MaskKey:4/binary, PayloadData/binary>>, State, {Socket, SocketMode, WsHandleLoopPid}) when PayloadLen < 126 andalso PayloadLen =< size(PayloadData) ->
	handle_frame(Fin, Opcode, PayloadLen, MaskKey, PayloadData, State#state{mask_key = MaskKey}, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<Fin:1, 0:3, Opcode:4, 1:1, 126:7, PayloadLen:16, MaskKey:4/binary, PayloadData/binary>>, State, {Socket, SocketMode, WsHandleLoopPid}) when PayloadLen =< size(PayloadData) ->
	handle_frame(Fin, Opcode, PayloadLen, MaskKey, PayloadData, State#state{mask_key = MaskKey}, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<Fin:1, 0:3, Opcode:4, 1:1, 127:7, 0:1, PayloadLen:63, MaskKey:4/binary, PayloadData/binary>>, State, {Socket, SocketMode, WsHandleLoopPid}) when PayloadLen =< size(PayloadData) ->
	handle_frame(Fin, Opcode, PayloadLen, MaskKey, PayloadData, State#state{mask_key = MaskKey}, {Socket, SocketMode, WsHandleLoopPid});
i_handle_data(<<_Fin:1, 0:3, _Opcode:4, 0:1, _PayloadLen:7, _Data/binary>>, _State, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	?LOG_DEBUG("client to server message was not sent masked, close websocket",[]),
	{websocket_close, websocket_close_data()};
i_handle_data(Data, #state{buffer = none} = State, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	State#state{buffer = Data}.

% handle frames
-spec handle_frame(
	Fin::integer(),
	Opcode::integer(),
	PayloadLen::integer(),
	MaskKey::binary(),
	PayloadData::binary(),
	State::term(),
	{Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | term().
handle_frame(1, ?OP_CONT, _Len, _MaskKey, _Data, _State, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	?LOG_WARNING("received an unsupported segment ~p, closing websocket", [{1, ?OP_CONT}]),
	{websocket_close, websocket_close_data()};
handle_frame(1, Opcode, Len, MaskKey, Data, State, {Socket, SocketMode, WsHandleLoopPid}) ->
	% frame without segment
	<<Data1:Len/binary, Rest/binary>> = Data,
	case Opcode of
		?OP_BIN ->
			handle_frame_received_msg(MaskKey, Data1, Rest, State, {Socket, SocketMode, WsHandleLoopPid});
		?OP_TEXT ->
			handle_frame_received_msg(MaskKey, Data1, Rest, State, {Socket, SocketMode, WsHandleLoopPid});
		?OP_PING ->
			% ping
			misultin_socket:send(Socket, send_format(Data1, ?OP_PONG, State), SocketMode),
			handle_frame_continue(Rest, State, {Socket, SocketMode, WsHandleLoopPid});
		?OP_CLOSE ->
			?LOG_DEBUG("received a websocket close request",[]),
			websocket_close;
		_OpOther ->
			?LOG_DEBUG("received segment with the unknown OpCode ~p, closing websocket", [_OpOther]),
			{websocket_close, websocket_close_data()}
	end;
% first frame of a segment, TODO: comply to multiple segments
handle_frame(0, _Opcode, _Len, _MaskKey, _Data, _State, {_Socket, _SocketMode, _WsHandleLoopPid}) ->
	?LOG_WARNING("received an unsupported continuation segment with opcode ~p, closing websocket", [{0, _Opcode}]),
	{websocket_close, websocket_close_data()}.

% received a message, send to websocket process
-spec handle_frame_received_msg(
	MaskKey::binary(),
	Data::binary(),
	Rest::binary(),
	State::term(),
	{Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | #state{}.
handle_frame_received_msg(MaskKey, Data, Rest, State, {Socket, SocketMode, WsHandleLoopPid}) ->
	Unmasked = binary_to_list(unmask(MaskKey, Data)),
	?LOG_DEBUG("received message from client: ~p", [Unmasked]),
	misultin_websocket:send_to_browser(WsHandleLoopPid, Unmasked),
	handle_frame_continue(Rest, State, {Socket, SocketMode, WsHandleLoopPid}).

% continue with rest of data
-spec handle_frame_continue(
	Rest::binary(),
	State::term(),
	{Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | #state{}.
handle_frame_continue(Rest, State, {Socket, SocketMode, WsHandleLoopPid}) ->
	case Rest of
		<<>> -> State#state{buffer = none};
		_ -> i_handle_data(Rest, State#state{buffer = none}, {Socket, SocketMode, WsHandleLoopPid})
	end.

% unmask
-spec unmask(Key::binary(), Data::binary()) -> binary(). 
unmask(Key, <<_:512,_Rest/binary>> = Data) ->
	K = binary:copy(Key, 512 div 32),
	<<LongKey:512>> = K,
	<<ShortKey:32>> = Key,
	unmask(ShortKey, LongKey, Data, <<>>);
unmask(Key, Data) ->
	<<ShortKey:32>> = Key,
	unmask(ShortKey,none, Data, <<>>).
unmask(Key, LongKey, Data, Accu) ->
	case Data of
		<<A:512, Rest/binary>> ->
			C = A bxor LongKey,
			unmask(Key, LongKey, Rest, <<Accu/binary, C:512>>);
		<<A:32,Rest/binary>> ->
			C = A bxor Key,
			unmask(Key, LongKey, Rest, <<Accu/binary, C:32>>);
		<<A:24>> ->
			<<B:24, _:8>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:24>>;
		<<A:16>> ->
			<<B:16, _:16>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:16>>;
		<<A:8>> ->
			<<B:8, _:24>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:8>>;
		<<>> ->
			Accu
	end.

% websocket close data
-spec websocket_close_data() -> binary().
websocket_close_data() ->
	<<1:1, 0:3, ?OP_CLOSE:4, 0:1, 0:7>>.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
