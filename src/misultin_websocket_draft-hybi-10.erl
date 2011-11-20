% ==========================================================================================================
% MISULTIN - WebSocket - draft hybi 10
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Richard Jones <rj@metabrew.com>, Roberto Ostinelli <roberto@ostinelli.net>,
%					  portions of code from Andy W. Song <https://github.com/awsong/erl_websocket>
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
-module('misultin_websocket_draft-hybi-10').
-behaviour(misultin_websocket).
-vsn("0.9-dev").

% API
-export([check_websocket/1, handshake/3, handle_data/3, send_format/2]).

% records
-record(state, {
	buffer	= <<>>,
	mask_key  = <<0,0,0,0>>,
	fragments = [] %% if we are in the midst of receving a fragmented message, fragments are contained here in reverse order
}).

-record(frame, {fin,
				rsv1,
				rsv2,
				rsv3,
				opcode,
				maskbit,
				length,
				maskkey,
				data}).

% macros
-define(OP_CONT, 0).
-define(OP_TEXT, 1).
-define(OP_BIN, 2).
-define(OP_CLOSE, 8).
-define(OP_PING, 9).
-define(OP_PONG, 10).

-define(IS_CONTROL_OPCODE(X), ((X band 8)=:=8) ).

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
		{'Upgrade', "websocket"}, {'Connection', "Upgrade"}, {'Host', ignore}, {'Sec-Websocket-Origin', ignore},
		{'Sec-Websocket-Key', ignore}, {'Sec-WebSocket-Version', "8"}
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
-spec handle_data(Data::binary(), 
				  Status::undefined | term(), 
				  {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> websocket_close | {websocket_close, binary()} | term().
handle_data(Data, St, Tuple) when is_list(Data) ->
	handle_data(list_to_binary(Data), St, Tuple);
handle_data(Data, undefined, {Socket, SocketMode, WsHandleLoopPid}) when is_binary(Data) ->
	% init status
	i_handle_data(#state{buffer = Data}, {Socket, SocketMode, WsHandleLoopPid});
handle_data(Data, State = #state{buffer=Buffer}, {Socket, SocketMode, WsHandleLoopPid}) when is_binary(Data) ->
	% read status
	i_handle_data(State#state{buffer = <<Buffer/binary, Data/binary>>}, {Socket, SocketMode, WsHandleLoopPid}).

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

%	   0				   1				   2				   3
%	   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%	  +-+-+-+-+-------+-+-------------+-------------------------------+
%	  |F|R|R|R| opcode|M| Payload len |	   Extended payload length	  |
%	  |I|S|S|S|	 (4)  |A|	  (7)	  |				(16/63)			  |
%	  |N|V|V|V|		  |S|			  |	  (if payload len==126/127)	  |
%	  | |1|2|3|		  |K|			  |								  |
%	  +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
%	  |		Extended payload length continued, if payload len == 127  |
%	  + - - - - - - - - - - - - - - - +-------------------------------+
%	  |								  |Masking-key, if MASK set to 1  |
%	  +-------------------------------+-------------------------------+
%	  | Masking-key (continued)		  |			 Payload Data		  |
%	  +-------------------------------- - - - - - - - - - - - - - - - +
%	  :						Payload Data continued ...				  :
%	  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%	  |						Payload Data continued ...				  |
%	  +---------------------------------------------------------------+


% ---------------------------- \/ frame parsing ------------------------------------------------------------

% parse received data and get the frames
-spec take_frame(Data::binary()) -> {#frame{} | undefined, Rest::binary()}.
% normal length
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 PayloadLen:7,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) when PayloadLen < 126 ->
	%% Don't auto-unmask control frames
	Data = case ?IS_CONTROL_OPCODE(Opcode) of
		true  -> PayloadData;
		false -> unmask(MaskKey,PayloadData)
	end,
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data = Data}, Rest};
% extende payload (126)
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 126:7,
			 PayloadLen:16,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) ->
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data=unmask(MaskKey,PayloadData)},	Rest};
% extende payload (127)
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 127:7, %% "If 127, the following 8 bytes interpreted as a 64-bit unsigned integer (the most significant bit MUST be 0)" 
			 0:1,	%% MSB of 0
			 PayloadLen:63,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) ->
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data=unmask(MaskKey, PayloadData)},	 Rest};
% catch wrong format
take_frame(Data) when is_binary(Data) ->
	{undefined, Data}.

% process incoming data
-spec i_handle_data(#state{}, {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> #state{} | {websocket_close, term()}.
i_handle_data(#state{buffer=ToParse} = State, {Socket, SocketMode, WsHandleLoopPid}) ->
	case take_frame(ToParse) of
		{undefined, Rest} ->
			?LOG_DEBUG("no frame to take, add to buffer: ~p", [Rest]),
			%% no full frame to be had yet
			State#state{buffer = Rest};
		{Frame=#frame{}, Rest} ->
			?LOG_DEBUG("parsed frame ~p, remaining buffer is: ~p", [Frame,Rest]),
			%% sanity check, in case client is broken
			case sanity_check(Frame) of
				ok ->
					?LOG_DEBUG("sanity checks successfully performed",[]),
					case handle_frame(Frame, 
									  State#state{buffer = Rest}, 
									  {Socket, SocketMode, WsHandleLoopPid}) of
						%% tail-call if there is stuff in the buffer still to parse
						NewState = #state{buffer = B} when is_binary(B), B =/= <<>> ->
							i_handle_data(NewState, {Socket, SocketMode, WsHandleLoopPid});
						Other ->
							Other
					end;
				protocol_error ->
					?LOG_DEBUG("sanity checks errors encountered, closing websocket",[]),
					{websocket_close, websocket_close_data()}
			end
	end.

% format sanity checks
-spec sanity_check(#frame{}) -> ok | protocol_error.
sanity_check(Frame) ->
	Checks = [
		{1, Frame#frame.maskbit},
		{0, Frame#frame.rsv1},
		{0, Frame#frame.rsv2},
		{0, Frame#frame.rsv3}
	],
	ChecksVerified = lists:dropwhile(
		fun ({A, A}) -> true;
			({_A, _}) -> false
		end, Checks
	),
	case ChecksVerified of
		[] -> ok;
		_ -> protocol_error
	end.

% ---------------------------- /\ frame parsing ------------------------------------------------------------

% ---------------------------- \/ fragment handling --------------------------------------------------------

-spec handle_frame(#frame{}, #state{}, {Socket::socket(), SocketMode::socketmode(), WsHandleLoopPid::pid()}) -> #state{} | {websocket_close, term()}.
%% FRAGMENT - add to the list and carry on
%% "A fragmented message consists of a single frame with the FIN bit
%%	clear and an opcode other than 0, followed by zero or more frames
%%	with the FIN bit clear and the opcode set to 0, and terminated by
%%	a single frame with the FIN bit set and an opcode of 0"
handle_frame(#frame{fin = 0, opcode = Opcode}, %% first fragment
			 State = #state{fragments = []} = Frame, 
			 _) when Opcode =/= ?OP_CONT ->
	?LOG_DEBUG("first fragment: ~p", [Frame]),
	State#state{fragments = [Frame]};
handle_frame(#frame{fin = 0, opcode = ?OP_CONT}, %% subsequent fragments
			 State = #state{fragments = Frags} = Frame, 
			 _) when Frags =/= [] ->
	?LOG_DEBUG("next fragment: ~p", [Frame]),
	State#state{fragments = [Frame | Frags]};

%% Last frame in a fragmented message.
%% reassemble one large frame based on all the fragments, keeping opcode from first:
handle_frame(#frame{fin = 1, opcode = ?OP_CONT } = F, 
			 State = #state{fragments = Frags}, 
			 {Socket, SocketMode, WsHandleLoopPid}) when Frags =/= [] ->
	[Frame1|Frames] = lists:reverse([F|Frags]),
	Frame = lists:foldl(
		fun(#frame{length = L, data = D}, AccF) ->
			%% NB: we unmask data as we parse frames, so concating here is ok:
			AccF#frame{length = (AccF#frame.length + L), data = << (AccF#frame.data)/binary, D/binary >>}
		end,
		Frame1#frame{fin=1},
		Frames
	),
	?LOG_DEBUG("final fragment, assembled: ~p",[Frame]),
	%% now process this new combined message as if we got it all at once:
	handle_frame(Frame, State#state{fragments = []}, {Socket, SocketMode, WsHandleLoopPid});

%% end of fragments but no fragments stored - error
handle_frame(#frame{fin = 1, opcode = ?OP_CONT}, _, _) ->
	%% closing, should only happen if client is broken
	{websocket_close, websocket_close_data()};

% ---------------------------- /\ fragment handling --------------------------------------------------------

% ---------------------------- \/ frame handling -----------------------------------------------------------

%% CONTROL FRAMES: 1) cannot be fragmented, thus have size <= 125bytes
%%				 2) have an opcode where MSB is set
%%				 3) can appear between larger fragmented message frames
handle_frame(#frame{fin=1, opcode=Opcode, data=Data}, 
			 State, 
			 {Socket, SocketMode, _WsHandleLoopPid}) when ?IS_CONTROL_OPCODE(Opcode) ->
	%% handle all known control opcodes:
	case Opcode of
		?OP_PING ->
			misultin_socket:send(Socket, send_format(Data, ?OP_PONG, State), SocketMode),
			State;
		?OP_CLOSE ->
			?LOG_DEBUG("received a websocket close request",[]),
			websocket_close;
		_OpOther ->
			?LOG_DEBUG("received segment with the unknown control OpCode ~p, closing websocket", [_OpOther]),
			{websocket_close, websocket_close_data()}
	end;

%% NORMAL FRAME (not a fragment, not a control frame)
handle_frame(#frame{fin=1, opcode=Opcode, data=Data},
			 State, 
			 {_Socket, _SocketMode, WsHandleLoopPid}) when Opcode =:= ?OP_BIN; Opcode =:= ?OP_TEXT ->
	misultin_websocket:send_to_browser(WsHandleLoopPid, binary_to_list(Data)),
	State.

% ---------------------------- /\ frame handling -----------------------------------------------------------

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
